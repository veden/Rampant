modName := `jq -r '.name' info.json`
modVersion := `jq -r '.version' info.json`
title := modName + "_" + modVersion
modFolder := "/mnt/gallery/gameFiles/factorio/mods"
zipPath := modFolder + "/" + title + ".zip"
ciZipPath := "./" + title + ".zip"
modPath := modFolder + "/" + title
serverPath := modFolder + "/" + title

_clearFiles:
    rm -f {{ zipPath }}
    rm -rf {{ modPath }}

_copyFiles path:
    mkdir -p {{ path }}
    cp -v ./*.lua {{ path }}
    cp -v ./*.png {{ path }}
    cp -v ./*.json {{ path }}
    cp -v -r ./sounds {{ path }}
    cp -v -r ./locale {{ path }}
    cp -v -r ./libs {{ path }}
    cp -v -r ./graphics {{ path }}
    cp -v -r ./prototypes {{ path }}
    cp -v -r ./migrations {{ path }}
    cp -v ./COPYING {{ path }}
    cp -v ./changelog.txt {{ path }}
    cp -v ./README.md {{ path }}

copy: _clearFiles (_copyFiles modPath)
    @echo "Copied to {{ modPath }}"

_zipFiles path:
    ln -s "$(pwd)" {{ title }}

    zip -r {{ path }} \
    {{ title }}/*.lua \
    {{ title }}/*.png \
    {{ title }}/*.json \
    {{ title }}/sounds/**/* \
    {{ title }}/locale/**/* \
    {{ title }}/libs/**/* \
    {{ title }}/graphics/**/* \
    {{ title }}/prototypes/**/* \
    {{ title }}/migrations/**/* \
    {{ title }}/COPYING \
    {{ title }}/changelog.txt \
    {{ title }}/README.md

    rm {{ title }}

zip: _clearFiles (_zipFiles zipPath)
    @echo "Zipped to {{ zipPath }}"

play:
    factorio

ci: (_zipFiles ciZipPath)
    #!/usr/bin/env -S bash
    echo "init uploading"

    initResponse=$(curl --no-progress-meter -X POST "https://mods.factorio.com/api/v2/mods/releases/init_upload" \
      -H "Authorization: Bearer $APIKEY" -H "Content-Type: multipart/form-data" \
      -F "mod={{ modName }}")

    if [[ $(echo "$initResponse" | jq -r '.error') != "null" ]]; then
      echo "init upload failed: $initResponse"
      exit 3
    fi

    echo "uploading"

    finishResponse=$(curl --no-progress-meter -X POST "$(echo "$initResponse" | jq -r '.upload_url')" \
      -H "Authorization: Bearer $APIKEY" -H "Content-Type: multipart/form-data" \
      -F "file=@{{ ciZipPath }}")

    if [[ $(echo "$finishResponse" | jq -r '.error') != "null" ]]; then
      echo "finish upload failed: $finishResponse"
      exit 4
    else
      echo "upload success: $finishResponse"
    fi

    rm -f ciZipPath

# Run integration tests
test:
    #!/usr/bin/env -S bash
    set -e
    echo "Running Rampant integration tests..."
    pkill -9 factorio 2>/dev/null || true
    sleep 1

    # Create private server settings
    echo '{"name":"Rampant Integration Tests","description":"Automated integration tests","visibility":{"public":false,"lan":false},"require_user_verification":false,"max_players":5}' > /tmp/rampant-test-server-settings.json

    rm -rf /mnt/gallery/gameFiles/factorio/scenarios/rampant-integration-tests
    cp -r integration-tests/scenario /mnt/gallery/gameFiles/factorio/scenarios/rampant-integration-tests

    # Start Factorio in background
    /mnt/gallery/gameFiles/factorio/bin/x64/factorio \
        --start-server-load-scenario rampant-integration-tests \
        --server-settings /tmp/rampant-test-server-settings.json \
        --port 34197 \
        --rcon-port 27015 \
        --rcon-password "rampant-test" \
        --console-log /tmp/rampant-test.log &
    PID=$!

    # Monitor log file for test completion
    echo "Waiting for tests to complete..."
    for i in {1..60}; do
        if grep -q "TESTS_COMPLETE\|TESTS_FAILED" /tmp/rampant-test.log 2>/dev/null; then
            sleep 2
            break
        fi
        sleep 1
    done

    # Show results
    echo ""
    echo "=== Test Results ==="
    grep -E "^\s*\[|^---|Test Summary|passed|failed|TESTS" /tmp/rampant-test.log 2>/dev/null | tail -30
    echo ""

    # Check pass/fail status
    if grep -q "TESTS_COMPLETE" /tmp/rampant-test.log 2>/dev/null; then
        echo "All integration tests passed!"
    else
        echo "Integration tests failed!"
        exit 1
    fi

    # Send quit command via RCON
    echo "Sending quit command via RCON..."
    (echo "quit"; sleep 1) | nc -w 2 localhost 27015 2>/dev/null || true

    # Give it a moment to shut down gracefully
    sleep 2

    # Force kill if still running
    if kill -0 $PID 2>/dev/null; then
        echo "Force-killing Factorio..."
        kill -9 $PID 2>/dev/null || true
    fi

    echo ""

# Copy mod and integration test scenario to Factorio
copy-all: copy
    #!/usr/bin/env -S bash
    set -e
    echo "Copying integration test scenario..."
    SCENARIO_PATH="$(pwd)/integration-tests/scenario"
    FACTORIO_SCENARIOS="/mnt/gallery/gameFiles/factorio/scenarios/rampant-integration-tests"
    rm -rf "$FACTORIO_SCENARIOS"
    mkdir -p "$FACTORIO_SCENARIOS"
    cp -r "$SCENARIO_PATH"/* "$FACTORIO_SCENARIOS/"
    echo "Test scenario copied to $FACTORIO_SCENARIOS"
    echo ""
    echo "To run tests:"
    echo "  just test"
    echo ""
    echo "Or to run tests manually:"
    echo "  factorio --start-server-load-scenario rampant-integration-tests"

# Run active AI integration tests (full initialization, speed=100)
test-active:
    #!/usr/bin/env -S bash
    set -e
    echo "Running Rampant Active AI tests..."
    pkill -9 factorio 2>/dev/null || true
    sleep 1

    # Create private server settings
    echo '{"name":"Rampant Active AI Tests","description":"Tests active AI behavior with event capture","visibility":{"public":false,"lan":false},"require_user_verification":false,"max_players":5}' > /tmp/rampant-active-test-server-settings.json

    rm -rf /mnt/gallery/gameFiles/factorio/scenarios/rampant-active-ai-tests
    cp -r integration-tests/active-ai-scenario /mnt/gallery/gameFiles/factorio/scenarios/rampant-active-ai-tests

    # Start Factorio in background
    /mnt/gallery/gameFiles/factorio/bin/x64/factorio \
        --start-server-load-scenario rampant-active-ai-tests \
        --server-settings /tmp/rampant-active-test-server-settings.json \
        --port 34199 \
        --rcon-port 27017 \
        --rcon-password "rampant-active-test" \
        --console-log /tmp/rampant-active-test.log &
    PID=$!

    # Monitor log file for test completion
    echo "Waiting for tests to complete..."
    for i in {1..120}; do
        if grep -q "TESTS_COMPLETE\|TESTS_FAILED" /tmp/rampant-active-test.log 2>/dev/null; then
            sleep 2
            break
        fi
        sleep 1
    done

    # Show results
    echo ""
    echo "=== Active AI Test Results ==="
    grep -E "^\s*\[|^---|Test Summary|passed|failed|TES" /tmp/rampant-active-test.log 2>/dev/null | tail -40
    echo ""

    # Check pass/fail status
    if grep -q "TESTS_COMPLETE" /tmp/rampant-active-test.log 2>/dev/null; then
        echo "Active AI integration tests completed!"
    else
        echo "Active AI integration tests failed or timed out!"
        exit 1
    fi

    # Send quit command via RCON
    echo "Sending quit command via RCON..."
    (echo "quit"; sleep 1) | nc -w 2 localhost 27017 2>/dev/null || true

    # Give it a moment to shut down gracefully
    sleep 2

    # Force kill if still running
    if kill -0 $PID 2>/dev/null; then
        echo "Force-killing Factorio..."
        kill -9 $PID 2>/dev/null || true
    fi

    echo ""
        if grep -q "TESTS_COMPLETE\|TESTS_FAILED" /tmp/rampant-ai-test.log 2>/dev/null; then
            sleep 2
            break
        fi
        sleep 1
    done

    # Show results
    echo ""
    echo "=== AI Response Test Results ==="
    grep -E "^\s*\[|^---|Test Summary|passed|failed|TESTS" /tmp/rampant-ai-test.log 2>/dev/null | tail -30
    echo ""

    # Check pass/fail status
    if grep -q "TESTS_COMPLETE" /tmp/rampant-ai-test.log 2>/dev/null; then
        echo "AI Response integration tests passed!"
    else
        echo "AI Response integration tests failed!"
        exit 1
    fi

    # Send quit command via RCON
    echo "Sending quit command via RCON..."
    (echo "quit"; sleep 1) | nc -w 2 localhost 27016 2>/dev/null || true

    # Give it a moment to shut down gracefully
    sleep 2

    # Force kill if still running
    if kill -0 $PID 2>/dev/null; then
        echo "Force-killing Factorio..."
        kill -9 $PID 2>/dev/null || true
    fi

    echo ""

# Run RCON-based AI tests
test-rcon:
    #!/usr/bin/env -S bash
    set -e
    echo "Running Rampant RCON AI tests..."
    pkill -9 factorio 2>/dev/null || true
    sleep 1

    # Copy scenario
    rm -rf /mnt/gallery/gameFiles/factorio/scenarios/rampant-active-ai-tests
    cp -r integration-tests/active-ai-scenario /mnt/gallery/gameFiles/factorio/scenarios/rampant-active-ai-tests

    # Run test script
    integration-tests/run-rcon-tests.sh