# Game database schema, version 1.
#
# STRICT tables enforce column types (an integer ply column refuses 1.5 or
# text). JSON columns are checked with json_valid(); JSON is stored only as
# data and never evaluated. Every state-bearing join carries the rules hash.
#
#   schema_migrations  applied migrations: version, SHA-256, time, versions
#   rules_profiles     exact rules identities (hash, version, schemas)
#   sources            provenance of imported bytes (one row per distinct
#                      SHA-256) and named app sources (no bytes)
#   games              one row per catalogued game: content hash, trajectory
#                      hash, summary, metadata, soft-delete marker
#   game_players       per colour: display name, agent, model revision
#   game_sources       provenance links; a game may have many
#   actions            one row per event (board actions and session events);
#                      `event_seq` is the event counter, `ply` the board ply
#   snapshots          validated initial and final states as canonical JSON
#   annotations        comments, tags and imported annotations
#   analysis_runs      analysis provenance (evaluator, model, settings hash)
#   evaluations        per-position results of an analysis run
#   quarantine         rejected records with a bounded diagnostic and the
#                      first invalid event/ply; the source is kept
#
# Content identity (`game_content_hash`) is `g3_game_hash()`: rules hash,
# initial state and every event including resignations and agreed draws.
# It is indexed, not unique: a live game saved by the app keeps its own
# identity while it grows, and the importer and saver look up identical
# content explicitly (see database-import.R). `trajectory_hash` covers only
# the active line of actions, so games with the same moves but different
# adjudications can be grouped without being merged.

#' @noRd
.g3_db_schema_v1 <- function() {
  c(
    "CREATE TABLE schema_migrations (
      version INTEGER PRIMARY KEY CHECK (version >= 1),
      name TEXT NOT NULL,
      checksum TEXT NOT NULL CHECK (length(checksum) = 64),
      applied_at TEXT NOT NULL,
      package_version TEXT NOT NULL,
      sqlite_version TEXT NOT NULL
    ) STRICT",

    "CREATE TABLE rules_profiles (
      rules_hash TEXT PRIMARY KEY CHECK (length(rules_hash) = 64),
      rules_version TEXT NOT NULL,
      action_schema TEXT NOT NULL,
      state_schema TEXT NOT NULL,
      registered_at TEXT NOT NULL
    ) STRICT",

    "CREATE TABLE sources (
      source_id INTEGER PRIMARY KEY,
      kind TEXT NOT NULL CHECK (kind IN ('file', 'app')),
      source_sha256 TEXT UNIQUE CHECK (source_sha256 IS NULL OR length(source_sha256) = 64),
      label TEXT NOT NULL CHECK (length(label) BETWEEN 1 AND 1000),
      location TEXT,
      format TEXT NOT NULL CHECK (format IN ('g3-json', 'g3-jsonl', 'app')),
      compression TEXT CHECK (compression IS NULL OR compression = 'gzip'),
      bytes INTEGER CHECK (bytes IS NULL OR bytes >= 0),
      record_count INTEGER CHECK (record_count IS NULL OR record_count >= 0),
      creator TEXT,
      licence TEXT,
      parser_version TEXT NOT NULL,
      status TEXT NOT NULL CHECK (status IN ('importing', 'complete', 'active')),
      import_count INTEGER NOT NULL DEFAULT 0 CHECK (import_count >= 0),
      first_seen_at TEXT NOT NULL,
      last_import_at TEXT NOT NULL,
      CHECK ((kind = 'file') = (source_sha256 IS NOT NULL))
    ) STRICT",
    "CREATE UNIQUE INDEX sources_app_label ON sources(label) WHERE kind = 'app'",

    "CREATE TABLE games (
      game_id TEXT PRIMARY KEY CHECK (length(game_id) BETWEEN 1 AND 128),
      origin_game_id TEXT,
      origin TEXT NOT NULL CHECK (origin IN ('import', 'app')),
      game_content_hash TEXT NOT NULL CHECK (length(game_content_hash) = 64),
      trajectory_hash TEXT NOT NULL CHECK (length(trajectory_hash) = 64),
      rules_hash TEXT NOT NULL REFERENCES rules_profiles(rules_hash),
      initial_state_hash TEXT NOT NULL CHECK (length(initial_state_hash) = 64),
      initial_ply INTEGER NOT NULL CHECK (initial_ply >= 0),
      final_ply INTEGER NOT NULL CHECK (final_ply >= 0),
      plies INTEGER NOT NULL CHECK (plies >= 0),
      n_events INTEGER NOT NULL CHECK (n_events >= 0),
      final_state_hash TEXT NOT NULL CHECK (length(final_state_hash) = 64),
      final_position_key TEXT NOT NULL,
      status TEXT NOT NULL CHECK (status IN ('live', 'finished')),
      result TEXT CHECK (result IS NULL OR result IN ('white', 'black', 'draw', 'truncated')),
      termination TEXT,
      game_date TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      revision INTEGER NOT NULL CHECK (revision >= 1),
      metadata_json TEXT NOT NULL CHECK (json_valid(metadata_json)),
      provenance_json TEXT CHECK (provenance_json IS NULL OR json_valid(provenance_json)),
      ui_json TEXT CHECK (ui_json IS NULL OR json_valid(ui_json)),
      deleted_at TEXT,
      deleted_note TEXT,
      CHECK ((status = 'live') = (result IS NULL))
    ) STRICT",
    "CREATE INDEX games_content ON games(game_content_hash)",
    "CREATE INDEX games_trajectory ON games(trajectory_hash)",
    "CREATE INDEX games_rules ON games(rules_hash)",
    "CREATE INDEX games_date ON games(game_date, game_id)",
    "CREATE INDEX games_updated ON games(updated_at, game_id)",
    "CREATE INDEX games_result ON games(result, termination)",
    "CREATE INDEX games_plies ON games(plies, game_id)",
    "CREATE INDEX games_deleted ON games(deleted_at) WHERE deleted_at IS NOT NULL",

    "CREATE TABLE game_players (
      game_id TEXT NOT NULL REFERENCES games(game_id) ON DELETE CASCADE,
      color TEXT NOT NULL CHECK (color IN ('white', 'black')),
      name TEXT NOT NULL,
      agent TEXT,
      model_revision TEXT,
      PRIMARY KEY (game_id, color)
    ) STRICT, WITHOUT ROWID",
    "CREATE INDEX game_players_name ON game_players(name COLLATE NOCASE)",
    "CREATE INDEX game_players_agent ON game_players(agent COLLATE NOCASE)",
    "CREATE INDEX game_players_model ON game_players(model_revision)",

    "CREATE TABLE game_sources (
      game_id TEXT NOT NULL REFERENCES games(game_id) ON DELETE CASCADE,
      source_id INTEGER NOT NULL REFERENCES sources(source_id),
      record_index INTEGER NOT NULL CHECK (record_index >= 0),
      relation TEXT NOT NULL CHECK (relation IN ('original', 'duplicate', 'save')),
      content_hash TEXT NOT NULL CHECK (length(content_hash) = 64),
      linked_at TEXT NOT NULL,
      PRIMARY KEY (source_id, record_index, game_id)
    ) STRICT",
    "CREATE UNIQUE INDEX game_sources_record ON game_sources(source_id, record_index) WHERE record_index > 0",
    "CREATE INDEX game_sources_game ON game_sources(game_id)",

    "CREATE TABLE actions (
      game_id TEXT NOT NULL REFERENCES games(game_id) ON DELETE CASCADE,
      event_seq INTEGER NOT NULL CHECK (event_seq >= 1),
      event_id TEXT NOT NULL,
      event_type TEXT NOT NULL CHECK (event_type IN ('action', 'resign', 'draw_agreed', 'truncated', 'takeback')),
      actor TEXT NOT NULL CHECK (actor IN ('white', 'black', 'system')),
      ply INTEGER NOT NULL CHECK (ply >= 0),
      action_id TEXT,
      action_type TEXT CHECK (action_type IS NULL OR action_type IN ('move', 'platform', 'castle')),
      notation TEXT,
      pre_state_hash TEXT NOT NULL CHECK (length(pre_state_hash) = 64),
      post_state_hash TEXT NOT NULL CHECK (length(post_state_hash) = 64),
      position_key TEXT,
      payload_json TEXT NOT NULL CHECK (json_valid(payload_json)),
      PRIMARY KEY (game_id, event_seq),
      CHECK ((event_type = 'action') = (action_id IS NOT NULL))
    ) STRICT",
    "CREATE INDEX actions_action ON actions(action_id) WHERE action_id IS NOT NULL",
    "CREATE INDEX actions_position ON actions(position_key) WHERE position_key IS NOT NULL",

    "CREATE TABLE snapshots (
      game_id TEXT NOT NULL REFERENCES games(game_id) ON DELETE CASCADE,
      kind TEXT NOT NULL CHECK (kind IN ('initial', 'final')),
      event_seq INTEGER NOT NULL CHECK (event_seq >= 0),
      ply INTEGER NOT NULL CHECK (ply >= 0),
      state_hash TEXT NOT NULL CHECK (length(state_hash) = 64),
      position_key TEXT NOT NULL,
      state_json TEXT NOT NULL CHECK (json_valid(state_json)),
      PRIMARY KEY (game_id, kind)
    ) STRICT",

    "CREATE TABLE annotations (
      annotation_id INTEGER PRIMARY KEY,
      game_id TEXT NOT NULL REFERENCES games(game_id) ON DELETE CASCADE,
      kind TEXT NOT NULL CHECK (kind IN ('comment', 'tag', 'imported')),
      ply INTEGER CHECK (ply IS NULL OR ply >= 0),
      body TEXT NOT NULL CHECK (length(body) BETWEEN 1 AND 65536),
      author TEXT,
      created_at TEXT NOT NULL,
      source_id INTEGER REFERENCES sources(source_id),
      deleted_at TEXT
    ) STRICT",
    "CREATE INDEX annotations_game ON annotations(game_id, kind)",
    "CREATE INDEX annotations_tag ON annotations(body) WHERE kind = 'tag' AND deleted_at IS NULL",

    "CREATE TABLE analysis_runs (
      run_id TEXT PRIMARY KEY CHECK (length(run_id) BETWEEN 1 AND 128),
      kind TEXT NOT NULL CHECK (kind IN ('position', 'game', 'database', 'comparison')),
      evaluator TEXT NOT NULL,
      model_revision TEXT,
      rules_hash TEXT NOT NULL REFERENCES rules_profiles(rules_hash),
      settings_json TEXT NOT NULL CHECK (json_valid(settings_json)),
      settings_hash TEXT NOT NULL CHECK (length(settings_hash) = 64),
      status TEXT NOT NULL CHECK (status IN ('running', 'completed', 'partial', 'failed', 'cancelled')),
      started_at TEXT NOT NULL,
      finished_at TEXT,
      package_version TEXT NOT NULL,
      provenance_json TEXT CHECK (provenance_json IS NULL OR json_valid(provenance_json))
    ) STRICT",

    "CREATE TABLE evaluations (
      evaluation_id INTEGER PRIMARY KEY,
      run_id TEXT NOT NULL REFERENCES analysis_runs(run_id) ON DELETE CASCADE,
      game_id TEXT REFERENCES games(game_id) ON DELETE CASCADE,
      event_seq INTEGER CHECK (event_seq IS NULL OR event_seq >= 0),
      ply INTEGER CHECK (ply IS NULL OR ply >= 0),
      state_hash TEXT NOT NULL CHECK (length(state_hash) = 64),
      analysis_context_hash TEXT NOT NULL CHECK (length(analysis_context_hash) = 64),
      perspective TEXT NOT NULL CHECK (perspective IN ('white', 'black', 'side_to_act')),
      score REAL CHECK (score IS NULL OR abs(score) <= 1e12),
      score_kind TEXT NOT NULL,
      mate_in INTEGER,
      p_win REAL CHECK (p_win IS NULL OR p_win BETWEEN 0 AND 1),
      p_draw REAL CHECK (p_draw IS NULL OR p_draw BETWEEN 0 AND 1),
      p_loss REAL CHECK (p_loss IS NULL OR p_loss BETWEEN 0 AND 1),
      best_action_id TEXT,
      played_action_id TEXT,
      depth INTEGER CHECK (depth IS NULL OR depth >= 0),
      nodes INTEGER CHECK (nodes IS NULL OR nodes >= 0),
      completeness TEXT,
      na_reason TEXT,
      payload_json TEXT CHECK (payload_json IS NULL OR json_valid(payload_json)),
      CHECK ((p_win IS NULL) = (p_draw IS NULL) AND (p_win IS NULL) = (p_loss IS NULL)),
      CHECK (p_win IS NULL OR abs(p_win + p_draw + p_loss - 1) < 1e-6)
    ) STRICT",
    "CREATE INDEX evaluations_run ON evaluations(run_id)",
    "CREATE INDEX evaluations_game ON evaluations(game_id, event_seq)",
    "CREATE INDEX evaluations_context ON evaluations(analysis_context_hash)",

    "CREATE TABLE quarantine (
      quarantine_id INTEGER PRIMARY KEY,
      source_id INTEGER NOT NULL REFERENCES sources(source_id),
      record_index INTEGER NOT NULL CHECK (record_index >= 1),
      record_sha256 TEXT CHECK (record_sha256 IS NULL OR length(record_sha256) = 64),
      condition TEXT NOT NULL,
      diagnostic TEXT NOT NULL CHECK (length(diagnostic) <= 1000),
      first_invalid_event INTEGER CHECK (first_invalid_event IS NULL OR first_invalid_event >= 1),
      first_invalid_ply INTEGER CHECK (first_invalid_ply IS NULL OR first_invalid_ply >= 0),
      claimed_game_id TEXT,
      claimed_rules_hash TEXT,
      record_text TEXT,
      first_seen_at TEXT NOT NULL,
      last_seen_at TEXT NOT NULL,
      UNIQUE (source_id, record_index)
    ) STRICT"
  )
}
