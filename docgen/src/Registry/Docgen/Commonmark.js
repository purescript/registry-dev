import * as commonmark from 'commonmark';

const absoluteUriRegex = /^(https|mailto):/i;

function isAllowedUri(uri) {
  // return uri.startsWith('#') || uri.startsWith('/') || absoluteUriRegex.test(uri);
  return absoluteUriRegex.test(uri);
}

function sanitize(md) {
  const walker = md.walker();

  while (true) {
    const event = walker.next();

    if (!event) {
      break;
    }

    const node = event.node;

    if (
      !event.entering &&
      node.type === 'link' &&
      node.destination &&
      !isAllowedUri(node.destination)
    ) {
      while (node.firstChild) {
        node.insertBefore(node.firstChild);
      }
      node.unlink();
    }
  }

  return md;
}

export function _renderMarkdownHTML(options, str) {
  const md = new commonmark.Parser().parse(str);

  if (options.safe) {
    sanitize(md);
  }

  return new commonmark.HtmlRenderer({ safe: options.safe, smart: true }).render(md);
}
