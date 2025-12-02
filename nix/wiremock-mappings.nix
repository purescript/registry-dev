# WireMock test mappings for integration tests
#
# These mappings simulate external services (GitHub API, S3, Pursuit) for
# the registry server integration tests.
#
# Usage in flake.nix:
#   let wiremockMappings = import ./nix/wiremock-mappings.nix; in
#   services.wiremock-github-api.mappings = wiremockMappings.github-api;
{
  github-api = [
    {
      request = {
        method = "GET";
        url = "/repos/purescript/purescript-effect/contents/bower.json?ref=v4.0.0";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/json";
        jsonBody = {
          type = "file";
          encoding = "base64";
          content = ''
            ewogICJuYW1lIjogInB1cmVzY3JpcHQtZWZmZWN0IiwKICAiaG9tZXBhZ2Ui
            OiAiaHR0cHM6Ly9naXRodWIuY29tL3B1cmVzY3JpcHQvcHVyZXNjcmlwdC1l
            ZmZlY3QiLAogICJsaWNlbnNlIjogIkJTRC0zLUNsYXVzZSIsCiAgInJlcG9z
            aXRvcnkiOiB7CiAgICAidHlwZSI6ICJnaXQiLAogICAgInVybCI6ICJodHRw
            czovL2dpdGh1Yi5jb20vcHVyZXNjcmlwdC9wdXJlc2NyaXB0LWVmZmVjdC5n
            aXQiCiAgfSwKICAiaWdub3JlIjogWwogICAgIioqLy4qIiwKICAgICJib3dl
            cl9jb21wb25lbnRzIiwKICAgICJub2RlX21vZHVsZXMiLAogICAgIm91dHB1
            dCIsCiAgICAidGVzdCIsCiAgICAiYm93ZXIuanNvbiIsCiAgICAicGFja2Fn
            ZS5qc29uIgogIF0sCiAgImRlcGVuZGVuY2llcyI6IHsKICAgICJwdXJlc2Ny
            aXB0LXByZWx1ZGUiOiAiXjYuMC4wIgogIH0KfQo='';
        };
      };
    }
    {
      request = {
        method = "GET";
        url = "/repos/purescript/purescript-effect/contents/LICENSE?ref=v4.0.0";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/json";
        jsonBody = {
          type = "file";
          encoding = "base64";
          content = ''
            Q29weXJpZ2h0IDIwMTggUHVyZVNjcmlwdAoKUmVkaXN0cmlidXRpb24gYW5k
            IHVzZSBpbiBzb3VyY2UgYW5kIGJpbmFyeSBmb3Jtcywgd2l0aCBvciB3aXRo
            b3V0IG1vZGlmaWNhdGlvbiwKYXJlIHBlcm1pdHRlZCBwcm92aWRlZCB0aGF0
            IHRoZSBmb2xsb3dpbmcgY29uZGl0aW9ucyBhcmUgbWV0OgoKMS4gUmVkaXN0
            cmlidXRpb25zIG9mIHNvdXJjZSBjb2RlIG11c3QgcmV0YWluIHRoZSBhYm92
            ZSBjb3B5cmlnaHQgbm90aWNlLCB0aGlzCmxpc3Qgb2YgY29uZGl0aW9ucyBh
            bmQgdGhlIGZvbGxvd2luZyBkaXNjbGFpbWVyLgoKMi4gUmVkaXN0cmlidXRp
            b25zIGluIGJpbmFyeSBmb3JtIG11c3QgcmVwcm9kdWNlIHRoZSBhYm92ZSBj
            b3B5cmlnaHQgbm90aWNlLAp0aGlzIGxpc3Qgb2YgY29uZGl0aW9ucyBhbmQg
            dGhlIGZvbGxvd2luZyBkaXNjbGFpbWVyIGluIHRoZSBkb2N1bWVudGF0aW9u
            IGFuZC9vcgpvdGhlciBtYXRlcmlhbHMgcHJvdmlkZWQgd2l0aCB0aGUgZGlz
            dHJpYnV0aW9uLgoKMy4gTmVpdGhlciB0aGUgbmFtZSBvZiB0aGUgY29weXJp
            Z2h0IGhvbGRlciBub3IgdGhlIG5hbWVzIG9mIGl0cyBjb250cmlidXRvcnMK
            bWF5IGJlIHVzZWQgdG8gZW5kb3JzZSBvciBwcm9tb3RlIHByb2R1Y3RzIGRl
            cml2ZWQgZnJvbSB0aGlzIHNvZnR3YXJlIHdpdGhvdXQKc3BlY2lmaWMgcHJp
            b3Igd3JpdHRlbiBwZXJtaXNzaW9uLgoKVEhJUyBTT0ZUV0FSRSBJUyBQUk9W
            SURFRCBCWSBUSEUgQ09QWVJJR0hUIEhPTERFUlMgQU5EIENPTlRSSUJVVE9S
            UyAiQVMgSVMiIEFORApBTlkgRVhQUkVTUyBPUiBJTVBMSUVEIFdBUlJBTlRJ
            RVMsIElOQ0xVRElORywgQlVUIE5PVCBMSU1JVEVEIFRPLCBUSEUgSU1QTElF
            RApXQVJSQU5USUVTIE9GIE1FUkNIQU5UQUJJTElUWSBBTkQgRklUTkVTUyBG
            T1IgQSBQQVJUSUNVTEFSIFBVUlBPU0UgQVJFCkRJU0NMQUlNRUQuIElOIE5P
            IEVWRU5UIFNIQUxMIFRIRSBDT1BZUklHSFQgSE9MREVSIE9SIENPTlRSSUJV
            VE9SUyBCRSBMSUFCTEUgRk9SCkFOWSBESVJFQ1QsIElORElSRUNULCBJTkNJ
            REVOVEFMLCBTUEVDSUFMLCBFWEVNUExBUlksIE9SIENPTlNFUVVFTlRJQUwg
            REFNQUdFUwooSU5DTFVESU5HLCBCVVQgTk9UIExJTUlURUQgVE8sIFBST0NV
            UkVNRU5UIE9GIFNVQlNUSVRVVEUgR09PRFMgT1IgU0VSVklDRVM7CkxPU1Mg
            T0YgVVNFLCBEQVRBLCBPUiBQUk9GSVRTOyBPUiBCVVNJTkVTUyBJTlRFUlJV
            UFRJT04pIEhPV0VWRVIgQ0FVU0VEIEFORCBPTgpBTlkgVEhFT1JZIE9GIExJ
            QUJJTElUWSwgV0hFVEhFUiBJTiBDT05UUkFDVCwgU1RSSUNUIExJQUJJTElU
            WSwgT1IgVE9SVAooSU5DTFVESU5HIE5FR0xJR0VOQ0UgT1IgT1RIRVJXSVNF
            KSBBUklTSU5HIElOIEFOWSBXQVkgT1VUIE9GIFRIRSBVU0UgT0YgVEhJUwpT
            T0ZUV0FSRSwgRVZFTiBJRiBBRFZJU0VEIE9GIFRIRSBQT1NTSUJJTElUWSBP
            RiBTVUNIIERBTUFHRS4K'';
        };
      };
    }
    {
      request = {
        method = "GET";
        url = "/repos/purescript/package-sets/tags";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/json";
        jsonBody = {
          name = "psc-0.15.10-20230105";
          commit = {
            sha = "090897c992b2b310b1456506308db789672adac1";
            url = "https://api.github.com/repos/purescript/package-sets/commits/090897c992b2b310b1456506308db789672adac1";
          };
        };
      };
    }
  ];

  # The S3 / bucket are set up such that prelude@6.0.1 has been uploaded
  # already, and the bucket gives a success response when uploading
  # the effect@4.0.0 package.
  s3-api = [
    {
      request = {
        method = "GET";
        url = "/prelude/6.0.1.tar.gz";
      };
      response = {
        status = 200;
        headers."Content-Type" = "application/octet-stream";
        bodyFileName = "prelude-6.0.1.tar.gz";
      };
    }
  ];

  bucket-api = [
    # List bucket contents
    {
      request = {
        method = "GET";
      };
      response = {
        status = 200;
        body = ''<ListBucketResult><Contents><Key>prelude/6.0.1.tar.gz</Key><Size>16298</Size><ETag>"abc123"</ETag></Contents></ListBucketResult>'';
      };
    }
    # We don't expect that effect-4.0.0 has been uploaded.
    {
      request = {
        method = "PUT";
        url = "/effect/4.0.0.tar.gz?x-id=PutObject";
      };
      response = {
        status = 200;
        body = ''<ETag>"abc123"</ETag>'';
      };
    }
    # But we do expect that prelude has been uploaded and can't be uploaded again.
    {
      request = {
        method = "PUT";
        url = "/prelude/6.0.1.tar.gz?x-id=PutObject";
      };
      response = {
        status = 500;
      };
    }
  ];

  pursuit-api = [
    # Already-published packages, ie. the registry-storage tarballs.
    {
      request = {
        method = "GET";
        url = "/packages/purescript-prelude/available-versions";
      };
      response = {
        status = 200;
        body = ''[["6.0.1","https://pursuit.purescript.org/packages/purescript-prelude/6.0.1"]]'';
      };
    }
    # The result of publishing a package, which we hardcode to 201 (success) for now.
    {
      request = {
        method = "POST";
        url = "/packages";
      };
      response = {
        status = 201;
      };
    }
  ];
}
