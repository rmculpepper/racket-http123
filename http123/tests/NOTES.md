
# Create key and certificate

    openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem \
      -days 365 -nodes \
      -subj "/CN=localhost" \
      -addext "subjectAltName=DNS:localhost,IP:127.0.0.1"

# Start the web server

    racket web-server.rkt

# Create reverse proxy

    nghttpx -b localhost,17180 -f '*,17190' --no-ocsp key.pem cert.pem

