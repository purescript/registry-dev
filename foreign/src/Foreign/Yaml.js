import Yaml from 'yaml';

export function yamlDocParserImpl(fail, succ, s) {
  let value;
  try {
    const doc = Yaml.parseDocument(s);
    if (doc.errors.length !== 0) {
      // TODO: eh, we should return properly structured errors here
      return fail(JSON.stringify(doc.errors));
    }
    Yaml.visit(doc, {
      Alias() {
        throw new Error('YAML aliases are not supported');
      }
    });
    value = doc.toJSON();
  } catch (error) {
    return fail(error instanceof Error ? error.message : String(error));
  }
  return succ(value);
}
