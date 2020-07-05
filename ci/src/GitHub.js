const { Octokit } = require("@octokit/rest");

if ("GITHUB_TOKEN" in process.env) {
  ;
} else {
  console.log('Please set GITHUB_TOKEN envvar');
  process.exit(1);
}

const octokit = new Octokit({ auth: process.env.GITHUB_TOKEN });

exports.getReleasesImpl = function (owner, repo) {
  return function () {
    return octokit.repos.listTags({
      owner,
      repo,
    }).then( ({ data }) => {
      return data.map(element => {
        return {
          name: element.name,
          sha: element.commit.sha
        };
      });
    });
  };
};

