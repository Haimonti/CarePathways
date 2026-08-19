const hopByHopHeaders = new Set([
  "connection",
  "content-encoding",
  "content-length",
  "host",
  "keep-alive",
  "proxy-authenticate",
  "proxy-authorization",
  "te",
  "trailer",
  "transfer-encoding",
  "upgrade",
]);

type RouteContext = {
  params: Promise<{ path?: string[] }>;
};

function backendOrigin() {
  const origin = process.env.BACKEND_ORIGIN;
  if (!origin) {
    throw new Error("Missing BACKEND_ORIGIN environment variable.");
  }
  return origin;
}

function responseHeaders(response: Response) {
  const headers = new Headers(response.headers);
  for (const header of hopByHopHeaders) headers.delete(header);
  return headers;
}

function requestHeaders(request: Request) {
  const headers = new Headers(request.headers);
  for (const header of hopByHopHeaders) headers.delete(header);
  return headers;
}

async function proxy(request: Request, context: RouteContext) {
  try {
    const params = await context.params;
    const incomingUrl = new URL(request.url);
    const pathname = `/${params.path?.join("/") ?? ""}`;
    const backendUrl = new URL(pathname, backendOrigin());
    backendUrl.search = incomingUrl.search;

    const init: RequestInit & { duplex?: "half" } = {
      method: request.method,
      headers: requestHeaders(request),
      redirect: "manual",
    };

    if (request.method !== "GET" && request.method !== "HEAD") {
      init.body = request.body;
      init.duplex = "half";
    }

    const response = await fetch(backendUrl, init);
    return new Response(response.body, {
      status: response.status,
      statusText: response.statusText,
      headers: responseHeaders(response),
    });
  } catch (error) {
    return Response.json(
      {
        detail:
          error instanceof Error ? error.message : "Next.js API proxy failed.",
      },
      { status: 502 },
    );
  }
}

export async function GET(request: Request, context: RouteContext) {
  return proxy(request, context);
}

export async function POST(request: Request, context: RouteContext) {
  return proxy(request, context);
}

export async function OPTIONS() {
  return new Response(null, { status: 204 });
}
