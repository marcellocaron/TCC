-- Inserir indice

INSERT INTO TB_INDICE (CD_INDICE, NM_INDICE)
VALUES
(39, 'T-BOND'),
(40, 'Market Risk Premium')

-- Inserir dados do RF

INSERT INTO
TB_INDICE_ANO (CD_INDICE, ANO, TX_INDICE)
VALUES
(39,2013,3.04),
(39,2014,2.17),
(39,2015,2.27),
(39,2016,2.45),
(39,2017,2.41),
(39,2018,2.68)

-- Inserir dados do RM

INSERT INTO
TB_INDICE_ANO (CD_INDICE, ANO, TX_INDICE)
VALUES
(40,2013,4.96),
(40,2014,5.78),
(40,2015,6.12),
(40,2016,5.69),
(40,2017,5.08),
(40,2018,5.96)