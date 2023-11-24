import Yaml from 'yaml';

export function yamlDocParserImpl(fail, succ, s) {
  const doc = Yaml.parseDocument(s);
  if (doc.errors.length === 0) {
    return succ(doc);
  } else {
    // TODO: eh, we should return properly structured errors here
    return fail(JSON.stringify(doc.errors));
  }
}

export function toJsonImpl(doc) {
  return doc.toJSON();
}
