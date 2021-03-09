FROM ghcr.io/purefunctor/site-frontend:latest as site-frontend

FROM nginx

# Copy dist folder into NGINX
COPY --from=site-frontend /static/ /usr/share/nginx/html/

# Copy configuration for NGINX
COPY ./docker/frontend/nginx.conf /etc/nginx/conf.d/default.conf
