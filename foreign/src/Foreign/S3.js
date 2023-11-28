import {
  DeleteObjectCommand,
  ListObjectsCommand,
  PutObjectCommand,
  S3,
} from "@aws-sdk/client-s3";

export const connectImpl = ({ key, secret }, endpoint) =>
  new S3({
    forcePathStyle: false,
    endpoint,
    region: "us-east-1",
    credentials: {
      accessKeyId: key,
      secretAccessKey: secret,
    },
  });

export const listObjectsImpl = async (s3, params) => {
  const data = await s3.send(new ListObjectsCommand(params));
  return data["Contents"];
};

export const putObjectImpl = async (s3, params) => {
  const data = await s3.send(new PutObjectCommand(params));
  return data;
};

export const deleteObjectImpl = async (s3, params) => {
  const data = await s3.send(new DeleteObjectCommand(params));
  return data;
};
