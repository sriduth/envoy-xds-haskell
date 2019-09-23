# Envoy XDS in Haskell

The base project for GRPC is cloned from: https://github.com/lucasdicioccio/warp-grpc


```
openssl genrsa -out key.pem 2048
openssl req -new -key key.pem -out certificate.csr
openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem
```

Then you can build and run the example.

```
stack build
stack exec -- envoy-xds
```

Now you can query it on 127.0.0.1:3000 with TLS, for instance with
https://github.com/lucasdicioccio/http2-client-grpc-example .

# Envoy XDS

Folder structure: 

* gen - Contains generated protobuf .hs files.
  Generate the .hs files from the following project: 
  https://github.com/sriduth/data-plane-api


# Configuration Options

Currently the system needs following parameters passed in as environment variables

	1) APP_MODE:
    
		Must be either 'FULL_XDS' or 'ACCESS_LOG'
   
	2) XDS_LOGS_PATH
	
	   Path where the XDS server can write it's own logs
	   
    3) GRPC_LOGS_PATH
	
	   Path to a folder where the requests to the XDS are logged.
	   
	4) ACCESS_LOGS_PATH
	
	   Path to folder where the access logs of envoy instances will be written.
	   Log files created will be in the format: `<ip-of-envoy-host>_access.log`
	   
### This implementation currently supports CDS and RDS functionalities in a very basic manner using GRPC.
