from __future__ import annotations

from datetime import datetime, timezone
import json
from pathlib import Path
import sqlite3
from typing import Dict, List, Optional

from ..schemas import RecordItem, ResultItem

DATASET_RECORDS_SQL = """
CREATE TABLE IF NOT EXISTS dataset_records (
    uuid INTEGER PRIMARY KEY,
    subject_id TEXT NOT NULL,
    hadm_id TEXT NOT NULL,
    input_text TEXT NOT NULL,
    actual_los_days REAL,
    admittime TEXT,
    dischtime TEXT,
    anchor_age REAL,
    gender TEXT,
    race TEXT,
    ethnicity TEXT,
    birthdate TEXT,
    admission_type TEXT,
    metadata_json TEXT,
    source_path TEXT,
    ingested_at TEXT NOT NULL
);
"""

PREDICTION_HISTORY_SQL = """
CREATE TABLE IF NOT EXISTS prediction_history (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    uuid INTEGER,
    subject_id TEXT,
    hadm_id TEXT,
    model_key TEXT NOT NULL,
    prediction_mode TEXT NOT NULL,
    predicted_los_days REAL NOT NULL,
    actual_los_days REAL,
    is_llos INTEGER NOT NULL,
    created_at TEXT NOT NULL
);
"""


class SQLiteRepository:
    def __init__(self, db_path: Path):
        self.db_path = db_path

    def _connect(self) -> sqlite3.Connection:
        self.db_path.parent.mkdir(parents=True, exist_ok=True)
        connection = sqlite3.connect(self.db_path)
        connection.row_factory = sqlite3.Row
        return connection

    def initialize(self) -> None:
        with self._connect() as connection:
            self._ensure_table_schema(
                connection=connection,
                table_name="dataset_records",
                required_columns={
                    "uuid",
                    "subject_id",
                    "hadm_id",
                    "input_text",
                    "actual_los_days",
                    "admittime",
                    "dischtime",
                    "anchor_age",
                    "gender",
                    "race",
                    "ethnicity",
                    "birthdate",
                    "admission_type",
                    "metadata_json",
                    "source_path",
                    "ingested_at",
                },
                create_sql=DATASET_RECORDS_SQL,
            )
            self._ensure_table_schema(
                connection=connection,
                table_name="prediction_history",
                required_columns={
                    "id",
                    "uuid",
                    "subject_id",
                    "hadm_id",
                    "model_key",
                    "prediction_mode",
                    "predicted_los_days",
                    "actual_los_days",
                    "is_llos",
                    "created_at",
                },
                create_sql=PREDICTION_HISTORY_SQL,
            )
            connection.commit()

    def _ensure_table_schema(
        self,
        connection: sqlite3.Connection,
        table_name: str,
        required_columns: set[str],
        create_sql: str,
    ) -> None:
        existing = connection.execute(
            "SELECT name FROM sqlite_master WHERE type = 'table' AND name = ?",
            (table_name,),
        ).fetchone()
        if existing is None:
            connection.execute(create_sql)
            return

        columns = {
            row["name"]
            for row in connection.execute(f"PRAGMA table_info({table_name})").fetchall()
        }
        if required_columns.issubset(columns):
            return

        backup_name = f"{table_name}_legacy_{int(datetime.now(timezone.utc).timestamp())}"
        connection.execute(f"ALTER TABLE {table_name} RENAME TO {backup_name}")
        connection.execute(create_sql)

    def is_ready(self) -> bool:
        try:
            with self._connect() as connection:
                connection.execute("SELECT 1")
            return True
        except sqlite3.Error:
            return False

    def upsert_dataset_record(
        self,
        uuid: int,
        subject_id: str,
        hadm_id: str,
        input_text: str,
        actual_los_days: float | None,
        admittime: str | None = None,
        dischtime: str | None = None,
        anchor_age: float | None = None,
        gender: str | None = None,
        race: str | None = None,
        ethnicity: str | None = None,
        birthdate: str | None = None,
        admission_type: str | None = None,
        metadata: Optional[Dict[str, object]] = None,
        source_path: str | None = None,
    ) -> None:
        ingested_at = datetime.now(timezone.utc).isoformat()
        with self._connect() as connection:
            connection.execute(
                """
                INSERT INTO dataset_records (
                    uuid,
                    subject_id,
                    hadm_id,
                    input_text,
                    actual_los_days,
                    admittime,
                    dischtime,
                    anchor_age,
                    gender,
                    race,
                    ethnicity,
                    birthdate,
                    admission_type,
                    metadata_json,
                    source_path,
                    ingested_at
                )
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ON CONFLICT(uuid) DO UPDATE SET
                    subject_id = excluded.subject_id,
                    hadm_id = excluded.hadm_id,
                    input_text = excluded.input_text,
                    actual_los_days = excluded.actual_los_days,
                    admittime = excluded.admittime,
                    dischtime = excluded.dischtime,
                    anchor_age = excluded.anchor_age,
                    gender = excluded.gender,
                    race = excluded.race,
                    ethnicity = excluded.ethnicity,
                    birthdate = excluded.birthdate,
                    admission_type = excluded.admission_type,
                    metadata_json = excluded.metadata_json,
                    source_path = excluded.source_path,
                    ingested_at = excluded.ingested_at
                """,
                (
                    uuid,
                    subject_id,
                    hadm_id,
                    input_text,
                    actual_los_days,
                    admittime,
                    dischtime,
                    anchor_age,
                    gender,
                    race,
                    ethnicity,
                    birthdate,
                    admission_type,
                    json.dumps(metadata or {}),
                    source_path,
                    ingested_at,
                ),
            )
            connection.commit()

    def get_dataset_record(self, uuid: int) -> Optional[Dict[str, object]]:
        with self._connect() as connection:
            row = connection.execute(
                """
                SELECT *
                FROM dataset_records
                WHERE uuid = ?
                """,
                (uuid,),
            ).fetchone()
        if row is None:
            return None
        return self._row_to_record_dict(row, include_input=True)

    def list_records(
        self,
        query: Optional[str],
        limit: int,
        offset: int,
    ) -> tuple[List[RecordItem], int]:
        filters = ""
        params: list[object] = []
        if query:
            filters = """
                WHERE CAST(uuid AS TEXT) LIKE ?
                   OR subject_id LIKE ?
                   OR hadm_id LIKE ?
            """
            like = f"%{query}%"
            params = [like, like, like]

        with self._connect() as connection:
            total_row = connection.execute(
                f"SELECT COUNT(*) FROM dataset_records {filters}",
                params,
            ).fetchone()
            total = int(total_row[0]) if total_row else 0
            rows = connection.execute(
                f"""
                SELECT *
                FROM dataset_records
                {filters}
                ORDER BY uuid ASC
                LIMIT ? OFFSET ?
                """,
                [*params, limit, offset],
            ).fetchall()

        return [self._row_to_record_item(row) for row in rows], total

    def insert_prediction(
        self,
        uuid: int | None,
        subject_id: str | None,
        hadm_id: str | None,
        model_key: str,
        prediction_mode: str,
        predicted_los_days: float,
        actual_los_days: float | None,
        is_llos: bool,
    ) -> ResultItem:
        created_at = datetime.now(timezone.utc).isoformat()
        with self._connect() as connection:
            cursor = connection.execute(
                """
                INSERT INTO prediction_history (
                    uuid,
                    subject_id,
                    hadm_id,
                    model_key,
                    prediction_mode,
                    predicted_los_days,
                    actual_los_days,
                    is_llos,
                    created_at
                )
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (
                    uuid,
                    subject_id,
                    hadm_id,
                    model_key,
                    prediction_mode,
                    predicted_los_days,
                    actual_los_days,
                    1 if is_llos else 0,
                    created_at,
                ),
            )
            connection.commit()
            prediction_id = int(cursor.lastrowid)

        return ResultItem(
            prediction_id=prediction_id,
            uuid=uuid,
            subject_id=subject_id,
            hadm_id=hadm_id,
            model_key=model_key,
            prediction_mode=prediction_mode,
            predicted_los_days=predicted_los_days,
            actual_los_days=actual_los_days,
            is_llos=is_llos,
            created_at=datetime.fromisoformat(created_at),
        )

    def list_predictions(self, limit: int, offset: int) -> List[ResultItem]:
        with self._connect() as connection:
            rows = connection.execute(
                """
                SELECT id, uuid, subject_id, hadm_id, model_key, prediction_mode,
                       predicted_los_days, actual_los_days, is_llos, created_at
                FROM prediction_history
                ORDER BY datetime(created_at) DESC, id DESC
                LIMIT ? OFFSET ?
                """,
                (limit, offset),
            ).fetchall()

        return [
            ResultItem(
                prediction_id=int(row["id"]),
                uuid=int(row["uuid"]) if row["uuid"] is not None else None,
                subject_id=row["subject_id"],
                hadm_id=row["hadm_id"],
                model_key=row["model_key"],
                prediction_mode=row["prediction_mode"],
                predicted_los_days=float(row["predicted_los_days"]),
                actual_los_days=float(row["actual_los_days"]) if row["actual_los_days"] is not None else None,
                is_llos=bool(row["is_llos"]),
                created_at=datetime.fromisoformat(row["created_at"]),
            )
            for row in rows
        ]

    def _row_to_record_item(self, row: sqlite3.Row) -> RecordItem:
        return RecordItem(
            uuid=int(row["uuid"]),
            subject_id=row["subject_id"],
            hadm_id=row["hadm_id"],
            admittime=row["admittime"],
            dischtime=row["dischtime"],
            actual_los_days=float(row["actual_los_days"]) if row["actual_los_days"] is not None else None,
            anchor_age=float(row["anchor_age"]) if row["anchor_age"] is not None else None,
            gender=row["gender"],
            race=row["race"],
            ethnicity=row["ethnicity"],
            admission_type=row["admission_type"],
        )

    def _row_to_record_dict(self, row: sqlite3.Row, include_input: bool = False) -> Dict[str, object]:
        record = self._row_to_record_item(row).model_dump()
        record["birthdate"] = row["birthdate"]
        record["metadata"] = json.loads(row["metadata_json"] or "{}")
        record["source_path"] = row["source_path"]
        record["ingested_at"] = row["ingested_at"]
        if include_input:
            record["input_text"] = row["input_text"]
        return record
