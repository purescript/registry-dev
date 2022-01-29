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
  return octokit
    .paginate(octokit.repos.listTags, {
      owner,
      repo,
    })
    .then((data) =>
      data.map((element) => {
        return { name: element.name, sha: element.commit.sha };
      })
    );
};

exports.getRefCommitImpl = function (octokit, { owner, repo }, ref) {
  return octokit.rest.git
    .getRef({ owner, repo, ref })
    .then((data) => data.object.sha);
};

exports.getCommitDateImpl = function (octokit, { owner, repo }, sha) {
  return octokit.rest.git
    .getCommit({ owner, repo, commit_sha: sha })
    .then(({ data }) => data.committer.date);
};

exports.closeIssueImpl = function (octokit, { owner, repo }, issue_number) {
  return octokit.issues.update({
    owner,
    repo,
    issue_number,
    state: "closed",
  });
};

exports.createCommentImpl = function (octokit, { owner, repo }, issue_number, body) {
  return octokit.issues.createComment({ owner, repo, issue_number, body });
};
