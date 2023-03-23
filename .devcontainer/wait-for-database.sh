#!/bin/sh

set -e

while getopts "d:" opt; do
    case $opt in
        d) database+=("$OPTARG");;
    esac
done
shift $((OPTIND -1))

cmd="$@"

for database in "${database[@]}"; do
  # checking if database folder exists instead of connecting to mysql which would require to install mysql client in container
  until [ -d /dbs/$database ]; do
    echo "MySQL database $database not yet created - sleeping"
    sleep 1
  done
done

echo "MySQL database created - executing command"
exec $cmd
