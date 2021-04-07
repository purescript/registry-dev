const { Octokit } = require("@octokit/rest");

if ("GITHUB_TOKEN" in process.env) {
} else {
  console.log("Please set GITHUB_TOKEN envvar");
  process.exit(1);
}

const octokit = new Octokit({ auth: process.env.GITHUB_TOKEN });

exports.getReleasesImpl = function ({ owner, repo }) {
  return octokit.repos
    .listTags({
      owner,
      repo,
    })
    .then(({ data }) => {
      return data.map((element) => {
        return {
          name: element.name,
          sha: element.commit.sha,
        };
      });
    });
};

exports.closeIssueImpl = function ({ owner, repo }, issue_number) {
  return octokit.issues.update({
    owner,
    repo,
    issue_number,
    state: "closed"
  });
}
exports.createCommentImpl = function ({ owner, repo }, issue_number, body) {
  return octokit.issues.createComment({ owner, repo, issue_number, body });
};
