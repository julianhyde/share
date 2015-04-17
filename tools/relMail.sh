#!/bin/bash
# Generates emails for Apache release votes

export REL=1.2.0-incubating
export REL2="1.2.0 (incubating)"
export RC=1
export BR=branch-1.2
export NEXUS=1007
export HASH=d60f2aa3aa9ce7cda7c4f6986af5729d50a28679

export RELNOTES_URL=https://github.com/apache/incubator-calcite/blob/${BR}/doc/HISTORY.md
export COMMIT_URL=http://git-wip-us.apache.org/repos/asf/incubator-calcite/commit/${HASH}
export NEXUS_URL=https://repository.apache.org/content/repositories/orgapachecalcite-${NEXUS}
export ARTIFACTS_URL=https://dist.apache.org/repos/dist/dev/incubator/calcite/apache-calcite-${REL}-rc${RC}
export KEY_URL=https://people.apache.org/keys/committer/jhyde.asc

checkUrl() {
  pushd /tmp
  echo check "$1"
  if curl -f -s -O "$1"; then
    :
  else
    echo bad url "$1"
    exit 1
  fi
  popd
}

checkUrl $RELNOTES_URL
checkUrl $COMMIT_URL
checkUrl $NEXUS_URL
checkUrl $ARTIFACTS_URL/apache-calcite-${REL}-src.tar.gz
checkUrl $KEY_URL

foo() {
  curl -s ${ARTIFACTS_URL}/apache-calcite-${REL}-${1}
}

mail1() {
  echo "To: dev@calcite.incubator.apache.org"
  echo "Subject: [VOTE] Release apache-calcite-${REL} (release candidate ${RC})"
  echo ""
  echo "Hi all,"
  echo ""
  echo "I have created a build for Apache Calcite ${REL}, release candidate ${RC}."
  echo ""
  echo "Thanks to everyone who has contributed to this release."
  echo "<Further details about release.> You can read the release notes here:"
  echo "${RELNOTES_URL}"
  echo ""
  echo "The commit to be voted upon:"
  echo "${COMMIT_URL}"
  echo ""
  echo "Its hash is ${HASH}."
  echo ""
  echo "The artifacts to be voted on are located here:"
  echo "${ARTIFACTS_URL}"
  echo ""
  echo "The hashes of the artifacts are as follows:"
  echo "src.tar.gz.md5 $(foo src.tar.gz.md5)"
  echo "src.tar.gz.sha1 $(foo src.tar.gz.sha1)"
  echo "src.zip.md5 $(foo src.zip.md5)"
  echo "src.zip.sha1 $(foo src.zip.sha1)"
  echo ""
  echo "A staged Maven repository is available for review at:"
  echo "${NEXUS_URL}"
  echo ""
  echo "Release artifacts are signed with the following key:"
  echo "${KEY_URL}"
  echo ""
  echo "Please vote on releasing this package as Apache Calcite ${REL}."
  echo ""
  echo "The vote is open for the next 72 hours and passes if a majority of"
  echo "at least three +1 PPMC votes are cast."
  echo ""
  echo "[ ] +1 Release this package as Apache Calcite ${REL}"
  echo "[ ]  0 I don't feel strongly about it, but I'm okay with the release"
  echo "[ ] -1 Do not release this package because..."
  echo ""
  echo ""
  echo "Here is my vote:"
  echo ""
  echo "+1 (binding)"
  echo ""
  echo "Julian"
}

mail2() {
  echo "Subject: [RESULT] [VOTE] Release apache-calcite-${REL} (release candidate ${RC})"
  echo "To: dev@calcite.incubator.apache.org"
  echo ""
  echo "Thanks to everyone who has tested the release candidate and given"
  echo "their comments and votes."
  echo ""
  echo "The tally is as follows."
  echo ""
  echo "N binding +1s:"
  echo "<names>"
  echo ""
  echo "N non-binding +1s:"
  echo "<names>"
  echo ""
  echo "No 0s or -1s."
  echo ""
  echo "Therefore I am delighted to announce that the proposal to release"
  echo "Apache Calcite ${REL} has passed."
  echo ""
  echo "I'll now start a vote on the general list. Those of you in the IPMC,"
  echo "please recast your vote on the new thread."
  echo ""
  echo "Julian"
}

mail3() {
  echo "To: general@incubator.apache.org"
  echo "Subject: [VOTE] Release apache-calcite-${REL}"
  echo ""
  echo "Hi all,"
  echo ""
  echo "The Calcite community has voted on and approved a proposal to release"
  echo "Apache Calcite ${REL}."
  echo ""
  echo "Proposal:"
  echo "http://mail-archives.apache.org/mod_mbox/incubator-calcite-dev/201408.mbox/MESSAGE-URI"
  echo ""
  echo "Vote result:"
  echo "N binding +1 votes"
  echo "N non-binding +1 votes"
  echo "No -1 votes"
  echo "http://mail-archives.apache.org/mod_mbox/incubator-calcite-dev/201408.mbox/MESSAGE-URI"
  echo ""
  echo "The commit to be voted upon:"
  echo "${COMMIT_URL}"
  echo ""
  echo "Its hash is ${HASH}."
  echo ""
  echo "The artifacts to be voted on are located here:"
  echo "${ARTIFACTS_URL}"
  echo ""
  echo "The hashes of the artifacts are as follows:"
  echo "src.tar.gz.md5 $(foo src.tar.gz.md5)"
  echo "src.tar.gz.sha1 $(foo src.tar.gz.sha1)"
  echo "src.zip.md5 $(foo src.zip.md5)"
  echo "src.zip.sha1 $(foo src.zip.sha1)"
  echo ""
  echo "A staged Maven repository is available for review at:"
  echo "${NEXUS_URL}"
  echo ""
  echo "Release artifacts are signed with the following key:"
  echo "${KEY_URL}"
  echo ""
  echo "Pursuant to the Releases section of the Incubation Policy and with"
  echo "the endorsement of NNN of our mentors we would now like to request"
  echo "the permission of the Incubator PMC to publish the release. The vote"
  echo "is open for 72 hours, or until the necessary number of votes (3 +1)"
  echo "is reached."
  echo ""
  echo "[ ] +1 Release this package as Apache Calcite ${REL}"
  echo "[ ] -1 Do not release this package because..."
  echo ""
  echo "Julian Hyde, on behalf of Apache Calcite PPMC"
}

mail4() {
  echo "To: general@incubator.apache.org"
  echo "Subject: [RESULT] [VOTE] Release apache-calcite-${REL}"
  echo ""
  echo "This vote passes with N +1s and no 0 or -1 votes:"
  echo "+1 <name> (mentor)"
  echo ""
  echo "There was some feedback during voting. I shall open a separate"
  echo "thread to discuss."
  echo ""
  echo "Thanks everyone. We’ll now roll the release out to the mirrors."
  echo ""
  echo "Julian"
}

mail5() {
  echo "To: announce@apache.org"
  echo "Subject: [ANNOUNCE] Apache Calcite ${REL2} released"
  echo ""
  echo "The Apache Calcite team is pleased to announce the release of"
  echo "Apache Calcite ${REL2}."
  echo ""
  echo "Calcite is a dynamic data management framework. Its cost-based"
  echo "optimizer converts queries, represented in relational algebra,"
  echo "into executable plans. Calcite supports many front-end languages"
  echo "and back-end data engines, and includes an SQL parser and the"
  echo "Avatica JDBC driver."
  echo ""
  echo "The release is available here:"
  echo ""
  echo "   http://www.apache.org/dyn/closer.cgi/incubator/calcite/apache-calcite-${REL}/"
  echo ""
  echo "We welcome your help and feedback. For more information on how to"
  echo "report problems, and to get involved, visit the project website at"
  echo ""
  echo "   http://calcite.incubator.apache.org"
  echo ""
  echo "Julian Hyde, on behalf of the Apache Calcite Team"
  echo ""
  echo ""
  echo "Disclaimer: Apache Calcite is an effort undergoing incubation at The"
  echo "Apache Software Foundation (ASF), sponsored by Apache Incubator."
  echo "Incubation is required of all newly accepted projects until a further"
  echo "review indicates that the infrastructure, communications, and decision"
  echo "making process have stabilized in a manner consistent with other"
  echo "successful ASF projects. While incubation status is not necessarily a"
  echo "reflection of the completeness or stability of the code, it does"
  echo "indicate that the project has yet to be fully endorsed by the ASF."
}

mail1 > mail1.txt
mail2 > mail2.txt
mail3 > mail3.txt
mail4 > mail4.txt
mail5 > mail5.txt

