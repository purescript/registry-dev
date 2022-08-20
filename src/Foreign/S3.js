import aws from "aws-sdk";

export function connectImpl(endpoint) {
  if ("SPACES_KEY" in process.env && "SPACES_SECRET" in process.env) {
  } else {
    console.log("Please set SPACES_KEY and SPACES_SECRET envvars");
    process.exit(1);
  }

  return function () {
    const spacesEndpoint = new aws.Endpoint(endpoint);
    const s3 = new aws.S3({
      endpoint: spacesEndpoint,
      accessKeyId: process.env.SPACES_KEY,
      secretAccessKey: process.env.SPACES_SECRET,
    });
    return s3;
  };
}

export function listObjectsImpl(s3, params) {
  return function () {
    return new Promise(function (resolve, reject) {
      s3.listObjectsV2(params, function (err, data) {
        if (err) {
          reject(err);
        } else {
          resolve(data["Contents"]);
        }
      });
    });
  };
}

export function putObjectImpl(s3, params) {
  return function () {
    return new Promise(function (resolve, reject) {
      s3.putObject(params, function (err, data) {
        if (err) {
          reject(err);
        } else {
          resolve(data);
        }
      });
    });
  };
}

export function deleteObjectImpl(s3, params) {
  return function () {
    return new Promise(function (resolve, reject) {
      s3.deleteObject(params, function (err, data) {
        if (err) {
          reject(err);
        } else {
          resolve(data);
        }
      });
    });
  };
}
