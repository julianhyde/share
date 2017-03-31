#!/bin/bash
# Generates emails for Apache release votes

export REL=1.6.0
export RC=0
export BR=branch-1.6
export NEXUS=1011
export HASH=3d12a4f0dddb51e8f8468d4d9b65f9880e25609a

export RELNOTES_URL=https://github.com/apache/calcite/blob/${BR}/site/_docs/history.md
export COMMIT_URL=http://git-wip-us.apache.org/repos/asf/calcite/commit/${HASH}
export NEXUS_URL=https://repository.apache.org/content/repositories/orgapachecalcite-${NEXUS}
export ARTIFACTS_URL=https://dist.apache.org/repos/dist/dev/calcite/apache-calcite-${REL}-rc${RC}
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
  echo "To: dev@calcite.apache.org"
  echo "Subject: [VOTE] Release apache-calcite-${REL} (release candidate ${RC})"
  echo ""
  echo "Hi all,"
  echo ""
  echo "I have created a build for Apache Calcite ${REL}, release candidate ${RC}."
  echo ""
  echo "Thanks to everyone who has contributed to this release."
  echo "<Further details about release.>"
  echo "You can read the release notes here:"
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
  echo "$(foo src.tar.gz.mds)"
  echo "$(foo src.zip.mds)"
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
  echo "at least three +1 PMC votes are cast."
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
  echo "To: dev@calcite.apache.org"
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
  echo "There was some feedback during voting. I shall open a separate"
  echo "thread to discuss."
  echo ""
  echo "Thanks everyone. We’ll now roll the release out to the mirrors."
  echo ""
  echo "Julian"
}

mail3() {
  echo "To: announce@apache.org"
  echo "Subject: [ANNOUNCE] Apache Calcite ${REL} released"
  echo ""
  echo "The Apache Calcite team is pleased to announce the release of"
  echo "Apache Calcite ${REL}."
  echo ""
  echo "Calcite is a dynamic data management framework. Its cost-based"
  echo "optimizer converts queries, represented in relational algebra,"
  echo "into executable plans. Calcite supports many front-end languages"
  echo "and back-end data engines, and includes an SQL parser and the"
  echo "Avatica JDBC driver."
  echo ""
  echo "The release is available here:"
  echo ""
  echo "   http://www.apache.org/dyn/closer.cgi/calcite/apache-calcite-${REL}/"
  echo ""
  echo "We welcome your help and feedback. For more information on how to"
  echo "report problems, and to get involved, visit the project website at"
  echo ""
  echo "   http://calcite.apache.org"
  echo ""
  echo "Julian Hyde, on behalf of the Apache Calcite Team"
}

mail1 > mail1.txt
mail2 > mail2.txt
mail3 > mail3.txt

# End relMail.sh
