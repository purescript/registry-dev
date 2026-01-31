import {
  DeleteObjectCommand,
  DeleteObjectsCommand,
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
  return data["Contents"] || [];
};

export const putObjectImpl = async (s3, params) => {
  const data = await s3.send(new PutObjectCommand(params));
  return data;
};

export const deleteObjectImpl = async (s3, params) => {
  const data = await s3.send(new DeleteObjectCommand(params));
  return data;
};

// Delete all objects in a bucket (lists with pagination, deletes in batches of 1000)
export const deleteAllObjectsImpl = async (s3, params) => {
  let totalDeleted = 0;
  let marker = undefined;

  do {
    // List up to 1000 objects
    const listCommand = new ListObjectsCommand({
      Bucket: params.Bucket,
      Marker: marker,
    });
    const listData = await s3.send(listCommand);
    const contents = listData.Contents || [];

    if (contents.length === 0) break;

    // Delete this batch
    const deleteCommand = new DeleteObjectsCommand({
      Bucket: params.Bucket,
      Delete: {
        Objects: contents.map((obj) => ({ Key: obj.Key })),
        Quiet: true,
      },
    });
    await s3.send(deleteCommand);
    totalDeleted += contents.length;

    // Continue if truncated
    marker = listData.IsTruncated
      ? listData.NextMarker || contents[contents.length - 1]?.Key
      : undefined;
  } while (marker);

  return { deleted: totalDeleted };
};
