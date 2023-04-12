import ssh2 from "ssh2";

export function parseKeyImpl(left, right, buffer, passphrase) {
  const parsed = ssh2.utils.parseKey(buffer, passphrase);
  if (parsed && parsed.type && parsed.comment) {
    return right(parsed);
  } else {
    return left(parsed);
  }
}

export function signImpl(parsedKey, buffer) {
  return parsedKey.sign(buffer);
}

export function verifyImpl(parsedKey, buffer, signature) {
  return parsedKey.verify(buffer, signature);
}

export function keyTypeImpl(parsedKey) {
  return parsedKey.type;
}

export function isPrivateKeyImpl(parsedKey) {
  return parsedKey.isPrivateKey();
}

export function equalsImpl(a, b) {
  return a.equals(b);
}
