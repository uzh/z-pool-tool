#!/bin/bash
# Shared function to test inotify support on the workspace filesystem

test_inotify_support() {
    local test_file="/workspace/pool/test_inotify_$$"
    echo "test" > "$test_file"
    timeout 2 inotifywait -e modify "$test_file" --timeout 1 >/dev/null 2>&1 &
    local inotify_pid=$!
    sleep 0.5
    echo "modified" >> "$test_file"
    sleep 0.5
    wait $inotify_pid 2>/dev/null
    local result=$?
    rm -f "$test_file"
    return $result
}

test_inotify_support
