const { Octokit } = require("@octokit/rest");

exports.mkOctokit = function () {
  if ("GITHUB_TOKEN" in process.env) {
  } else {
    console.log("Please set GITHUB_TOKEN envvar");
    process.exit(1);
  }

  return new Octokit({ auth: process.env.GITHUB_TOKEN });
};

exports.getReleasesImpl = function (octokit, { owner, repo }) {
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

exports.closeIssueImpl = function (octokit, { owner, repo }, issue_number) {
  return octokit.issues.update({
    owner,
    repo,
    issue_number,
    state: "closed"
  });
}
exports.createCommentImpl = function (octokit, { owner, repo }, issue_number, body) {
  return octokit.issues.createComment({ owner, repo, issue_number, body });
};
