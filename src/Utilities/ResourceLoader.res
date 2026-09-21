type loadResult = Loaded | Reused

type resource =
  | Script
  | Stylesheet

type adapter = {
  find: string => option<Dom.element>,
  create: (string, array<(string, string)>) => Dom.element,
  attach: Dom.element => unit,
}

@val @scope(("document", "head"))
external appendToHead: Dom.element => unit = "appendChild"

@val @scope("Array") external arrayFrom: array<'value> => array<'value> = "from"

let statusAttribute = "data-hyper-load-status"

let resourceName = resource =>
  switch resource {
  | Script => "script"
  | Stylesheet => "stylesheet"
  }

let resourceIdentity = url => url->String.replaceRegExp(/[?#].*$/, "")

let elementWithUrl = (~selector, ~attribute, ~url, ~ignoreQuery) => {
  let expectedUrl = ignoreQuery ? url->resourceIdentity : url
  Window.querySelectorAll(selector)
  ->arrayFrom
  ->Array.find(element =>
    element
    ->Window.getAttribute(attribute)
    ->Nullable.toOption
    ->Option.map(elementUrl =>
      (ignoreQuery ? elementUrl->resourceIdentity : elementUrl) === expectedUrl
    )
    ->Option.getOr(false)
  )
}

let withAttributes = (element, attributes) => {
  attributes->Array.forEach(((name, value)) => element->Window.setAttribute(name, value))
  element
}

let scriptAdapter = (~matchQuery) => {
  find: url =>
    elementWithUrl(~selector="script[src]", ~attribute="src", ~url, ~ignoreQuery=!matchQuery),
  create: (url, attributes) => {
    let script = Window.createElement("script")
    script->Window.setAttribute("type", "text/javascript")
    script->Window.setAttribute("src", url)
    script->withAttributes(attributes)
  },
  attach: script => Window.body->Window.appendChild(script),
}

let stylesheetAdapter = {
  find: url =>
    elementWithUrl(
      ~selector="link[rel=stylesheet][href]",
      ~attribute="href",
      ~url,
      ~ignoreQuery=false,
    ),
  create: (url, attributes) => {
    let stylesheet = Window.createElement("link")
    stylesheet->Window.setAttribute("rel", "stylesheet")
    stylesheet->Window.setAttribute("href", url)
    stylesheet->withAttributes(attributes)
  },
  attach: appendToHead,
}

let adapter = (resource, ~matchQuery) =>
  switch resource {
  | Script => scriptAdapter(~matchQuery)
  | Stylesheet => stylesheetAdapter
  }

let statusOf = element => element->Window.getAttribute(statusAttribute)->Nullable.toOption

let loadFailed = url => Exn.anyToExnInternal({"name": "RESOURCE_LOAD_ERROR", "message": url})

let load = (
  ~url,
  ~resource,
  ~attributes=[],
  ~matchQuery=false,
  ~dedupe=true,
  ~onStart=() => (),
  ~onLoad,
  ~onError,
) => {
  let adapter = resource->adapter(~matchQuery)

  let observe = (element, ~result) =>
    switch element->statusOf {
    | Some("ready") => onLoad(result)
    | Some("error") => onError(url->loadFailed)
    | _ => {
        element->Window.addLoadListener(() => {
          element->Window.setAttribute(statusAttribute, "ready")
          onLoad(result)
        })
        element->Window.addErrorListener(error => {
          element->Window.setAttribute(statusAttribute, "error")
          onError(error)
        })
      }
    }

  switch url->String.trim {
  | "" => onError(url->loadFailed)
  | _ =>
    switch dedupe ? adapter.find(url) : None {
    | Some(element) => setTimeout(() => element->observe(~result=Reused), 0)->ignore
    | None => {
        let element = adapter.create(url, attributes)
        element->Window.setAttribute(statusAttribute, "loading")
        onStart()
        element->observe(~result=Loaded)
        element->adapter.attach
      }
    }
  }
}
