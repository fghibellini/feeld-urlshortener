# feeld-shortener

```text
curl -v http://localhost:8081/shorten -H 'Content-type: application/json' -d '{ "url": "https://twitter.com/f_ghibellini" }'
...
< HTTP/1.1 200 OK
< Transfer-Encoding: chunked
< Date: Sat, 12 Jun 2021 19:30:55 GMT
< Server: Warp/3.3.14
< Content-Type: application/json;charset=utf-8
<
* Connection #0 to host localhost left intact
{"shortened":"http://localhost:8081/jp2tteuohj1zf6k3ihqu"}

curl -v http://localhost:8081/jp2tteuohj1zf6k3ihqu
...
< HTTP/1.1 301 Moved Permanently
< Transfer-Encoding: chunked
< Date: Sat, 12 Jun 2021 19:34:19 GMT
< Server: Warp/3.3.14
< Location: https://twitter.com/f_ghibellini
```

## To run

```
./start-db.sh
export BASE_URL="http://localhost:8081"
stack run
```

see [src/Feeld/UrlShortener/Config.hs](src/Feeld/UrlShortener/Config.hs) for all configuration options.

## To test

```
./start-db.sh
PGUSER=postgres PGPASSWORD=mysecretpassword PGHOST=localhost PGPORT=5432 stack test
```

## Build with Nix

```
nix-build -I nixpkgs=https://releases.nixos.org/nixpkgs/21.05-darwin/nixpkgs-darwin-21.05pre292449.e7c31a0eae7/nixexprs.tar.xz  ./package.nix
```
