import { S3 } from "@aws-sdk/client-s3";

export const connectImpl = ({ key, secret }, endpoint) => {
  const spacesEndpoint = new aws.Endpoint(endpoint);
  return new S3({
    endpoint: spacesEndpoint,
    accessKeyId: key,
    secretAccessKey: secret,
  });
};

export const listObjectsImpl = (s3, params) => {
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

export const putObjectImpl = (s3, params) => {
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

export const deleteObjectImpl = (s3, params) => {
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
