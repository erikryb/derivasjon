# Derivasjon

## Develop

```
cd code
nix-shell
```

## Deploy (Docker)
```
cd docker
nix-build
docker load -i result
docker run -p 8000:8000 derivasjon-image:[HASH FROM PREVIOUS COMMAND]
```

## Deploy (Nix)
```
cd nix
nix-build
./result/bin/derivasjon
```

## Page url

`http://localhost:8000/derivasjon/`
