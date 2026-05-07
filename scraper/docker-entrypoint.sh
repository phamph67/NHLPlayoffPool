#!/bin/sh
# Export Docker environment variables into a file that cron can source.
# This is necessary because cron does not inherit the container's environment.
printenv | grep -v '^_=' | sed "s/'/'\\\\''/g; s/^\([^=]*\)=\(.*\)/export \1='\2'/" > /app/env.sh

python /app/startup_check.py || exit 1

exec cron -f
