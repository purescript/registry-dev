var jsonic = require('jsonic');

exports.parseJsonicImpl = function (onError, onSuccess, input) {
  try {
    var parsedJson = jsonic(input)
    return onSuccess(parsedJson);
  } catch (e) {
    var reason = e && e.message;
    return onError(reason || "Unknown jsonic error");
  }
}
