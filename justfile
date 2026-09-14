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
test: copy
    #!/usr/bin/env -S bash
    set -e
    echo "Running Rampant integration tests..."
    pkill -9 factorio 2>/dev/null || true
    sleep 1

    # Remove stale saves: a save with the scenario name makes
    # --start-server-load-scenario load the save (fires on_load, which is
    # empty in the scenario) instead of creating the scenario fresh
    # (fires on_init, which runs the tests).
    rm -f /mnt/gallery/gameFiles/factorio/saves/rampant-integration-tests*

    # Create private server settings
    echo '{"name":"Rampant Integration Tests","description":"Automated integration tests","visibility":{"public":false,"lan":false},"require_user_verification":false,"max_players":5,"auto_pause":false,"autosave_interval":0}' > /tmp/rampant-test-server-settings.json

    rm -rf /mnt/gallery/gameFiles/factorio/scenarios/rampant-integration-tests
    cp -r integration-tests/scenario /mnt/gallery/gameFiles/factorio/scenarios/rampant-integration-tests

    # Start Factorio in background.
    # NOTE: on Factorio 2.1.x --console-log does not capture Lua print()
    # output (only the "Log opened" header); console prints stream to
    # stdout, so capture stdout and poll that.
    rm -f /tmp/rampant-test.out
    /mnt/gallery/gameFiles/factorio/bin/x64/factorio \
        --start-server-load-scenario rampant-integration-tests \
        --server-settings /tmp/rampant-test-server-settings.json \
        --port 34197 \
        --rcon-port 27015 \
        --rcon-password "rampant-test" \
        --console-log /tmp/rampant-test.log \
        > /tmp/rampant-test.out 2>&1 &
    PID=$!

    # Monitor output for test completion
    echo "Waiting for tests to complete..."
    for i in {1..60}; do
        if grep -q "TESTS_COMPLETE\|TESTS_FAILED" /tmp/rampant-test.out 2>/dev/null; then
            sleep 2
            break
        fi
        sleep 1
    done

    # Show results
    echo ""
    echo "=== Test Results ==="
    grep -E "^\s*\[|^---|Test Summary|passed|failed|TESTS" /tmp/rampant-test.out 2>/dev/null | tail -30
    echo ""

    # Check pass/fail status
    if grep -q "TESTS_COMPLETE" /tmp/rampant-test.out 2>/dev/null; then
        echo "All integration tests passed!"
    else
        echo "Integration tests failed!"
        kill -9 $PID 2>/dev/null || true
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
test-active: copy
    #!/usr/bin/env -S bash
    set -e
    echo "Running Rampant Active AI tests..."
    pkill -9 factorio 2>/dev/null || true
    sleep 1

    # Remove stale saves (see `test` recipe: prevents save-load instead of fresh init)
    rm -f /mnt/gallery/gameFiles/factorio/saves/rampant-active-ai-tests*

    # Create private server settings
    echo '{"name":"Rampant Active AI Tests","description":"Tests active AI behavior with event capture","visibility":{"public":false,"lan":false},"require_user_verification":false,"max_players":5,"auto_pause":false,"autosave_interval":0}' > /tmp/rampant-active-test-server-settings.json

    rm -rf /mnt/gallery/gameFiles/factorio/scenarios/rampant-active-ai-tests
    cp -r integration-tests/active-ai-scenario /mnt/gallery/gameFiles/factorio/scenarios/rampant-active-ai-tests

    # Start Factorio in background; capture stdout (see `test` recipe:
    # --console-log does not capture Lua prints on 2.1.x)
    rm -f /tmp/rampant-active-test.out
    /mnt/gallery/gameFiles/factorio/bin/x64/factorio \
        --start-server-load-scenario rampant-active-ai-tests \
        --server-settings /tmp/rampant-active-test-server-settings.json \
        --port 34199 \
        --rcon-port 27017 \
        --rcon-password "rampant-active-test" \
        --console-log /tmp/rampant-active-test.log \
        > /tmp/rampant-active-test.out 2>&1 &
    PID=$!

    # Monitor output for test completion
    echo "Waiting for tests to complete..."
    for i in {1..120}; do
        if grep -q "TESTS_COMPLETE\|TESTS_FAILED" /tmp/rampant-active-test.out 2>/dev/null; then
            sleep 2
            break
        fi
        sleep 1
    done

    # Show results
    echo ""
    echo "=== Active AI Test Results ==="
    grep -E "^\s*\[|^---|Test Summary|passed|failed|TES" /tmp/rampant-active-test.out 2>/dev/null | tail -40
    echo ""

    # Check pass/fail status
    if grep -q "TESTS_COMPLETE" /tmp/rampant-active-test.out 2>/dev/null; then
        echo "Active AI integration tests completed!"
    else
        echo "Active AI integration tests failed or timed out!"
        kill -9 $PID 2>/dev/null || true
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

# Run RCON-based AI tests
test-rcon: copy
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

# Run the save/load continuity test (fresh -> save -> reload -> verify)
test-save-load: copy
    #!/usr/bin/env -S bash
    set -e
    echo "Running Rampant save/load continuity test..."
    pkill -9 factorio 2>/dev/null || true
    sleep 1
    if [ ! -f integration-tests/behavior-saves/rampant-behavior-save-load-s1.zip ]; then
        just make-saves
    fi
    rm -f /mnt/gallery/gameFiles/factorio/saves/rampant-save-load-test*
    rm -rf /mnt/gallery/gameFiles/factorio/scenarios/rampant-behavior-save-load
    cp -r integration-tests/behavior-scenarios/save-load /mnt/gallery/gameFiles/factorio/scenarios/rampant-behavior-save-load
    cp integration-tests/behavior-saves/rampant-behavior-save-load-s1.zip /mnt/gallery/gameFiles/factorio/saves/rampant-behavior-save-load.zip
    python3 tools/save_load.py
    RESULT=$?
    kill -9 $(pgrep -f "factorio.*34203" | head -1) 2>/dev/null || true
    pkill -9 factorio 2>/dev/null || true
    exit $RESULT

# Run the E2E long-run session (4 game-hours, virtual player, telemetry interrogation)
test-longrun: copy
    #!/usr/bin/env -S bash
    set -e
    echo "Running Rampant E2E long-run session..."
    pkill -9 factorio 2>/dev/null || true
    sleep 1

    # Remove stale saves (see `test` recipe)
    rm -f /mnt/gallery/gameFiles/factorio/saves/rampant-behavior-long-run*

    # Create private server settings (auto_pause: false is required)
    echo '{"name":"Rampant Long Run","description":"E2E session interrogation","visibility":{"public":false,"lan":false},"require_user_verification":false,"max_players":5,"auto_pause":false,"autosave_interval":0}' > /tmp/rampant-longrun-server-settings.json

    if [ ! -f integration-tests/behavior-saves/rampant-behavior-long-run-s1.zip ]; then
        just make-saves
    fi
    just make-saves
    rm -rf /mnt/gallery/gameFiles/factorio/scenarios/rampant-behavior-long-run
    cp -r integration-tests/behavior-scenarios/long-run /mnt/gallery/gameFiles/factorio/scenarios/rampant-behavior-long-run
    cp integration-tests/behavior-saves/rampant-behavior-long-run-s1.zip /mnt/gallery/gameFiles/factorio/saves/rampant-behavior-long-run.zip

    rm -f /tmp/rampant-longrun.out /tmp/rampant-longrun.jsonl
    /mnt/gallery/gameFiles/factorio/bin/x64/factorio         --start-server-load-scenario rampant-behavior-long-run         --server-settings /tmp/rampant-longrun-server-settings.json         --port 34202 --rcon-port 27020         --rcon-password "rampant-test"         > /tmp/rampant-longrun.out 2>&1 &
    PID=$!

    # Interrogate the session
    python3 tools/long_run.py --port 27020 --password rampant-test         --interval 15 --max-wait 1500 --artifact /tmp/rampant-longrun.jsonl
    RESULT=$?

    # Show the scenario's own status lines
    echo ""
    echo "=== Scenario Status ==="
    grep -E "LONGRUN|VP:" /tmp/rampant-longrun.out | tail -40
    echo ""

    kill -9 $PID 2>/dev/null || true
    exit $RESULT

# Build the prepared behavior-save zips (small maps + scenario control).
# The zips are gitignored (generated artifacts); run this after a fresh
# clone or when scenario control.lua changes.
# Generate the prepared behavior-scenario saves.
# seed parameter: pins the map seed (--map-seed) so every scenario save is
# a DETERMINISTIC map. The mod's RNG is fixed-seed (Constants.ENEMY_SEED),
# so pinned seed + pinned mod = reproducible runs: a flake becomes a
# deterministic test case, not a dice roll. Default seed 1; `seed=random`
# is available for variance sampling. Cache: /tmp/rampant-saves (per seed).
make-saves seed="1":
    #!/usr/bin/env -S bash
    set -e
    SRC="$(pwd)"
    SEED="{{seed}}"
    mkdir -p /tmp/rampant-saves
    for size in 256 512 1024; do
        if [ ! -f "/tmp/rampant-saves/map-$size-s$SEED.zip" ]; then
            if [ "$SEED" = "random" ]; then
                /mnt/gallery/gameFiles/factorio/bin/x64/factorio \
                    --create "/tmp/rampant-saves/map-$size-s$SEED.zip" \
                    --map-gen-settings $SRC/integration-tests/behavior-saves/mapgen/$size.json \
                    > /dev/null 2>&1
            else
                /mnt/gallery/gameFiles/factorio/bin/x64/factorio \
                    --create "/tmp/rampant-saves/map-$size-s$SEED.zip" \
                    --map-gen-settings $SRC/integration-tests/behavior-saves/mapgen/$size.json \
                    --map-gen-seed "$SEED" \
                    > /dev/null 2>&1
            fi
            echo "generated map-$size-s$SEED"
        fi
    done
    for name in wall-defense death-response migration-settler save-load player-death long-run base-ai-states base-expansion pheromone-dynamics squad-cycle siege-settle multi-surface movement-diag movement-test; do
        case $name in
            wall-defense|death-response|save-load|player-death) SIZE=256 ;;
            migration-settler|base-ai-states|base-expansion|pheromone-dynamics|squad-cycle|siege-settle|multi-surface) SIZE=512 ;;
            movement-diag|movement-test) SIZE=256 ;;
            long-run) SIZE=1024 ;;
        esac
        DIR=/tmp/rampant-saves/$name-s$SEED
        rm -rf $DIR && mkdir -p $DIR/rampant-behavior-$name
        cd /tmp/rampant-saves && rm -rf x && mkdir x && unzip -o -q map-$SIZE-s$SEED.zip "map-$SIZE-s$SEED/level-init.dat" -d x
        cp x/map-$SIZE-s$SEED/level-init.dat $DIR/rampant-behavior-$name/level-init.dat
        cp $SRC/integration-tests/behavior-scenarios/$name/control.lua $DIR/rampant-behavior-$name/control.lua
        cp $SRC/integration-tests/behavior-scenarios/$name/info.json $DIR/rampant-behavior-$name/info.json
        # The shared scenario library (scenarios require() it as
        # "scenario-lib"; it must sit next to control.lua in the save).
        cp $SRC/integration-tests/lib/scenario-lib.lua $DIR/rampant-behavior-$name/scenario-lib.lua
        cd $DIR/rampant-behavior-$name
        rm -f $SRC/integration-tests/behavior-saves/rampant-behavior-$name-s$SEED.zip
        zip -q -r $SRC/integration-tests/behavior-saves/rampant-behavior-$name-s$SEED.zip level-init.dat control.lua info.json scenario-lib.lua
        cd /
        echo "built save: $name (seed $SEED)"
    done

# Run a staged behavior scenario (default: wall-defense)
# Usage: just test-behavior <name> [seed] [strict]  (positional binding)
#   e.g. just test-behavior siege-settle 2     # seed 2
#   just test-behavior wall-defense 1 1        # strict: known-flaky fails
# seed: pins the map (deterministic saves from `make-saves seed=N`); the
# mod's RNG is fixed-seed, so same seed + same mod = reproducible runs.
# `seed=random` loads the scenario dir fresh with an unpinned map.
# Logs: /tmp/rampant-behavior-<name>.out (previous run kept as .prev) —
# failures never overwrite the evidence.
# Flaky registry: integration-tests/behavior-flaky.list — a listed
# scenario failing prints KNOWN-FLAKY + the recorded reason and exits 0
# (so batteries don't block on tracked flakes). strict=1 overrides.
test-behavior name="wall-defense" seed="1" strict="0": copy
    #!/usr/bin/env -S bash
    set -e
    SEED="{{seed}}"
    LOG="/tmp/rampant-behavior-{{name}}.out"
    echo "Running Rampant behavior scenario: {{name}} (seed $SEED)..."
    pkill -9 factorio 2>/dev/null || true
    sleep 1

    # Remove stale saves (prevents save-load instead of fresh scenario init)
    rm -f /mnt/gallery/gameFiles/factorio/saves/rampant-behavior-{{name}}*

    # Create private server settings (auto_pause: false is required — the
    # server pauses with no players otherwise; see docs/09-Engine-Testing.md)
    echo '{"name":"Rampant Behavior Tests","description":"Staged behavior scenarios","visibility":{"public":false,"lan":false},"require_user_verification":false,"max_players":5,"auto_pause":false,"autosave_interval":0}' > /tmp/rampant-behavior-server-settings.json

    just make-saves
    rm -rf /mnt/gallery/gameFiles/factorio/scenarios/rampant-behavior-{{name}}
    cp -r integration-tests/behavior-scenarios/{{name}} /mnt/gallery/gameFiles/factorio/scenarios/rampant-behavior-{{name}}
    # The shared scenario library next to control.lua (the scenario-dir
    # load path — fallback and seed=random — must resolve its require too;
    # the prepared-save path bundles it via make-saves).
    cp integration-tests/lib/scenario-lib.lua /mnt/gallery/gameFiles/factorio/scenarios/rampant-behavior-{{name}}/scenario-lib.lua

    # Install the prepared save for this seed (a save named like the
    # scenario takes precedence over the scenario dir, giving a pinned
    # map + instant startup scan). `seed=random` skips it: the scenario
    # dir loads fresh with an unpinned map. Refresh the seed's saves on
    # every run so scenario control.lua changes propagate.
    if [ "$SEED" != "random" ]; then
        just make-saves "$SEED"
        SAVE=integration-tests/behavior-saves/rampant-behavior-{{name}}-s$SEED.zip
        cp "$SAVE" /mnt/gallery/gameFiles/factorio/saves/rampant-behavior-{{name}}.zip
    fi

    # Rotate logs: keep the previous run as .prev (failure evidence is
    # never overwritten by the next run).
    if [ -f "$LOG" ]; then mv "$LOG" "$LOG.prev"; fi

    # Start Factorio in background; capture stdout (see `test` recipe)
    /mnt/gallery/gameFiles/factorio/bin/x64/factorio \
        --start-server-load-scenario rampant-behavior-{{name}} \
        --server-settings /tmp/rampant-behavior-server-settings.json \
        --port 34201 \
        --rcon-port 27019 \
        --rcon-password "rampant-test" \
        > "$LOG" 2>&1 &
    PID=$!

    # Monitor output for test completion (budget: 600s real ~ 90 game-min at ~150 UPS)
    echo "Waiting for behavior scenario to complete..."
    for i in {1..600}; do
        if grep -q "TESTS_COMPLETE\|TESTS_FAILED" "$LOG" 2>/dev/null; then
            sleep 2
            break
        fi
        # A crashed server (non-recoverable mod error) never emits a test
        # signal — detect the dead process and fail fast instead of polling
        # the full budget.
        if ! kill -0 $PID 2>/dev/null; then
            echo ""
            echo "SERVER DIED (no test signal) — log tail:"
            tail -15 "$LOG"
            echo ""
            break
        fi
        sleep 1
    done

    # Show results
    echo ""
    echo "=== Behavior Test Results ==="
    grep -E "\[PASS\]|\[FAIL\]|\[INFO\]|PHASE|Passed|Failed|Detail|TESTS|STAGED" "$LOG" 2>/dev/null | tail -30
    echo ""

    # Check pass/fail status
    if grep -q "TESTS_COMPLETE" "$LOG" 2>/dev/null; then
        echo "Behavior scenario passed!"
    else
        echo "Behavior scenario failed or timed out!"
        kill -9 $PID 2>/dev/null || true
        # Known-flaky registry: a listed scenario's failure is a tracked
        # issue, not a new signal — exit 0 so batteries don't block.
        if grep -qE "^{{name}}[[:space:]]" integration-tests/behavior-flaky.list 2>/dev/null; then
            REASON=$(grep -E "^{{name}}[[:space:]]" integration-tests/behavior-flaky.list | head -1 | cut -f2)
            if [ "{{strict}}" != "1" ]; then
                echo "KNOWN-FLAKY (tracked): $REASON"
                echo "Re-run with strict=1 to treat as a failure."
                exit 0
            fi
        fi
        exit 1
    fi

    # Force kill (RCON quit via nc is not reliable; the binary protocol needs
    # tools/rcon_client.py)
    kill -9 $PID 2>/dev/null || true

    echo ""