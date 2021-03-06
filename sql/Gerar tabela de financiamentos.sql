--DROP TABLE aux.FINANCIAMENTO

/* ==================

SELECT *
FROM aux.FINANCIAMENTO

================== */

UPDATE aux.FINANCIAMENTO SET
VL_FINANCIAMENTO = REPLACE(VL_FINANCIAMENTO,'.','')


ALTER TABLE aux.FINANCIAMENTO ALTER COLUMN VL_FINANCIAMENTO BIGINT

ALTER TABLE aux.FINANCIAMENTO ALTER COLUMN ANO INT

DELETE FROM aux.FINANCIAMENTO
WHERE ANO IN (0,2012)

ALTER TABLE aux.FINANCIAMENTO ALTER COLUMN CD_EMPRESA INT

ALTER TABLE aux.FINANCIAMENTO ALTER COLUMN CD_INDICE INT

UPDATE aux.FINANCIAMENTO SET
CD_INDICE = NULL
WHERE CD_INDICE = 0


UPDATE aux.FINANCIAMENTO SET
TX_MULTIPLICADOR_INDICE = REPLACE(TX_MULTIPLICADOR_INDICE,',','.')

UPDATE aux.FINANCIAMENTO SET
TX_MULTIPLICADOR_INDICE = NULL
WHERE TX_MULTIPLICADOR_INDICE = ' '

ALTER TABLE aux.FINANCIAMENTO ALTER COLUMN TX_MULTIPLICADOR_INDICE DECIMAL(20,10)

UPDATE F SET
TX_MULTIPLICADOR_INDICE = 100
FROM aux.FINANCIAMENTO F
WHERE CD_INDICE > 0
AND TX_MULTIPLICADOR_INDICE IS NULL

UPDATE aux.FINANCIAMENTO SET
TX_ADICIONAL = REPLACE(TX_ADICIONAL,',','.')

UPDATE aux.FINANCIAMENTO SET
TX_ADICIONAL = NULL
WHERE TX_ADICIONAL = ' '

ALTER TABLE aux.FINANCIAMENTO ALTER COLUMN TX_ADICIONAL DECIMAL(20,10)


ALTER TABLE aux.FINANCIAMENTO ALTER COLUMN CD_INDICE_2 INT

UPDATE aux.FINANCIAMENTO SET
CD_INDICE_2 = NULL
WHERE CD_INDICE_2 = 0


UPDATE aux.FINANCIAMENTO SET
TX_MULTIPLICADOR_INDICE_2 = REPLACE(TX_MULTIPLICADOR_INDICE_2,',','.')

UPDATE aux.FINANCIAMENTO SET
TX_MULTIPLICADOR_INDICE_2 = NULL
WHERE TX_MULTIPLICADOR_INDICE_2 = ' '

ALTER TABLE aux.FINANCIAMENTO ALTER COLUMN TX_MULTIPLICADOR_INDICE_2 DECIMAL(20,10)

UPDATE F SET
TX_MULTIPLICADOR_INDICE_2 = 100
FROM aux.FINANCIAMENTO F
WHERE CD_INDICE_2 > 0
AND TX_MULTIPLICADOR_INDICE_2 IS NULL



UPDATE aux.FINANCIAMENTO SET
TX_ADICIONAL_2 = REPLACE(TX_ADICIONAL_2,',','.')

UPDATE aux.FINANCIAMENTO SET
TX_ADICIONAL_2 = NULL
WHERE TX_ADICIONAL_2 = ' '

ALTER TABLE aux.FINANCIAMENTO ALTER COLUMN TX_ADICIONAL_2 DECIMAL(20,10)

ALTER TABLE aux.FINANCIAMENTO ALTER COLUMN CD_INDICE_FIM INT

UPDATE aux.FINANCIAMENTO SET
CD_INDICE_FIM = NULL
WHERE CD_INDICE_FIM = 0


UPDATE aux.FINANCIAMENTO SET
TX_MULTIPLICADOR_INDICE_FIM = REPLACE(TX_MULTIPLICADOR_INDICE_FIM,',','.')

UPDATE aux.FINANCIAMENTO SET
TX_MULTIPLICADOR_INDICE_FIM = NULL
WHERE TX_MULTIPLICADOR_INDICE_FIM = ' '

ALTER TABLE aux.FINANCIAMENTO ALTER COLUMN TX_MULTIPLICADOR_INDICE_FIM DECIMAL(20,10)

UPDATE F SET
TX_MULTIPLICADOR_INDICE_FIM = 100
FROM aux.FINANCIAMENTO F
WHERE CD_INDICE_FIM > 0
AND TX_MULTIPLICADOR_INDICE_FIM IS NULL


UPDATE aux.FINANCIAMENTO SET
TX_ADICIONAL_FIM = REPLACE(TX_ADICIONAL_FIM,',','.')

UPDATE aux.FINANCIAMENTO SET
TX_ADICIONAL_FIM = NULL
WHERE TX_ADICIONAL_FIM = ' '

ALTER TABLE aux.FINANCIAMENTO ALTER COLUMN TX_ADICIONAL_FIM DECIMAL(20,10)

SELECT *
INTO dbo.TB_FINANCIAMENTO
FROM aux.FINANCIAMENTO