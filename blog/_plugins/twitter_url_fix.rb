# Fixes for jekyll-twitter-plugin 2.1.0 talking to Twitter/X's oEmbed API.

# Fix 1: the oEmbed endpoint moved. The plugin requests
# https://publish.twitter.com/oembed, which now returns "301 Moved
# Permanently" redirecting to https://publish.x.com/oembed. The plugin's
# ApiClient#fetch issues a single GET and does not follow redirects, so
# every tweet renders as "There was a 'Moved Permanently' error fetching
# URL". Override ApiRequest#to_uri to request the new host directly.
module TwitterJekyll
  class ApiRequest
    OEMBED_API_URL = "https://publish.x.com/oembed".freeze

    def to_uri
      URI.parse(OEMBED_API_URL).tap do |uri|
        uri.query = URI.encode_www_form url_params
      end
    end
  end
end

# Fix 2: the plugin unconditionally caches all responses, including
# errors. A transient network failure gets written to .tweet-cache and
# the error HTML is served on every subsequent build. Override
# live_response to only cache successful oEmbed responses (which always
# include a "url" key).
module TwitterJekyll
  class TwitterTag
    private

    def live_response
      if response = api_client.fetch(@api_request)
        cache.write(@api_request.cache_key, response) if response.key?("url")
        build_response(response)
      end
    end
  end
end
