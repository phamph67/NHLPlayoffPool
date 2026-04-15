#!/bin/sh
# Export Docker environment variables into a file that cron can source.
# This is necessary because cron does not inherit the container's environment.
printenv | grep -v '^_=' | sed "s/'/'\\\\''/g; s/=\(.*\)/='\1'/" > /app/env.sh
exec cron -f
