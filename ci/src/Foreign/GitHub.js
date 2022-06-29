const { Octokit: GitHubOctokit } = require("@octokit/rest");
const { retry } = require("@octokit/plugin-retry");
const { throttling } = require("@octokit/plugin-throttling");
const Octokit = GitHubOctokit.plugin(retry, throttling);

exports.mkOctokit = function (authToken) {
  const octokit = new Octokit({
    auth: authToken,
    request: {
      per_page: 100, // this is the maximum
    },
    throttle: {
      onRateLimit: (retryAfter, options) => {
        octokit.log.warn(
          `Request quota exhausted for request ${options.method} ${options.url}`
        );

        // Retry twice after hitting a rate limit error, then give up
        if (options.request.retryCount <= 2) {
          console.log(`Retrying after ${retryAfter} seconds!`);
          return true;
        }
      },
      onAbuseLimit: (_, options) => {
        octokit.log.error(
          `Abuse detected for request ${options.method} ${options.url}`
        );
      },
    },
  });
  return octokit;
};

// exports.request = function (octokit, route, headers, onError, onSuccess) {
//   return octokit
//     .request(route, {
//       headers: headers,
//     })
//     .then((data) => onSuccess(data))
//     .catch((err) => onError(err));
// };

// exports.paginate = function (octokit, route, headers, onError, onSuccess) {
//   return octokit
//     .paginate(route, {
//       headers: headers,
//     })
//     .then((data) => onSuccess(data))
//     .catch((err) => onError(err));
// };

/* Ideally, we would implement this using conditional requests and read releases
   from cache if we get a 304 response. This would reduce the number of requests
   made dramatically. However, octokit doesn't provide a way to access request
   headers, so this isn't yet implemented.
*/
exports.getReleasesImpl = function (octokit, { owner, repo }, left, right) {
  return octokit
    .paginate(octokit.repos.listTags, {
      owner,
      repo,
      per_page: 100, // maximum is 100, minimizes requests made
    })
    .then((data) => {
      const tags = data.map((element) => {
        return { name: element.name, sha: element.commit.sha };
      });
      return right(tags);
    })
    .catch((e) => {
      return left(e);
    });
};

exports.getRefCommitImpl = function (
  octokit,
  { owner, repo },
  ref,
  left,
  right
) {
  return octokit.rest.git
    .getRef({ owner, repo, ref })
    .then((data) => {
      return right(data.object.sha);
    })
    .catch((e) => {
      return left(e);
    });
};

exports.getCommitDateImpl = function (
  octokit,
  { owner, repo },
  sha,
  left,
  right
) {
  return octokit.rest.git
    .getCommit({ owner, repo, commit_sha: sha })
    .then(({ data }) => {
      return right(data.committer.date);
    })
    .catch((e) => {
      return left(e);
    });
};

exports.closeIssueImpl = function (
  octokit,
  { owner, repo },
  issue_number,
  left,
  right
) {
  return octokit.issues
    .update({
      owner,
      repo,
      issue_number,
      state: "closed",
    })
    .then(() => {
      return right(undefined);
    })
    .catch((e) => {
      return left(e);
    });
};

exports.createCommentImpl = function (
  octokit,
  { owner, repo },
  issue_number,
  body,
  left,
  right
) {
  return octokit.issues
    .createComment({ owner, repo, issue_number, body })
    .then(() => {
      return right(undefined);
    })
    .catch((e) => {
      return left(e);
    });
};
