*!* SELECT  Curtemp && Cursor antes
*!* SELECT  CurArt  && Cursor despues

lcPahtDBCom = oApp.RUTA

SELECT ;
    ca.*, ;
    a.grupo as grupo_com, ;
    a.grupo4, ;
    a.servicio, ;
    a.ecommerce, ;
    a.maki, ;
    a.idexterno ;
FROM ;
    CurArt ca ;
INNER JOIN articulos a ON a.codart = ca.codart ;
WHERE ;
    ca.sicambio = .T. ;
    AND (a.ecommerce = .T. OR a.maki = .T.) ;
    AND a.idexterno > 0 ;
    AND !EMPTY(a.grupo) ;
    AND !EMPTY(a.grupo4) ;
INTO CURSOR ;
    curResp

lcWait = ''

SELECT curResp
SCAN
	lcCodart = ALLTRIM(curResp.codart)
	lcServicio = curResp.servicio

	IF curResp.ecommerce
		SELECT cart.codart FROM articulo_comercio cart WHERE cart.codart == lcCodart AND NOT DELETED() INTO CURSOR curComer
		IF RECCOUNT('curComer') = 0
			lcWait = lcWait + ' - '+ curComer.codart + CHR(13) + CHR(10)
		ELSE
			subirMaki(curComer.codart, lcServicio)
		ENDIF
	ELSE
		subirMaki(lcCodart, lcServicio)
	ENDIF
ENDSCAN 

IF !EMPTY(lcWait)
	WAIT WINDOW "Los siguientes articulos no se sincronizaron (Revise el comercio): " + CHR(13) + CHR(10) + lcWait NOWAIT
ENDIF

*------------
*!* FUNCIONES
*------------
FUNCTION subirMaki(lcCodartAux, lcServicioAux)
	*!* -->	DECLARACION DEL EJECUTABLE PARA LA INTEGRACION
	     oShell = CREATEOBJECT("WScript.Shell")
	     lsRuta 	= 	["]+'C:\Fenix\AdsFenix\DATOS_pro\Datos_add\BR_eCommerce\API\InitIntegration.exe'+["]
	     lsBD	=	["]+ALLTRIM(lcPahtDBCom)+["]
    *!*	<-- DECLARACION DEL EJECUTABLE PARA LA INTEGRACION

	lcCodArt = ALLTRIM(lcCodartAux)
	UPDATE articulos SET sync = .F., update = .T. WHERE alltrim(codart) = lcCodArt
	
    *!*	DECLARACION DEL EJECUTABLE PARA LA INTEGRACION
	lsTable =	["]+'articulos'+["] 
	lsCode	=	["]+lcCodArt+["]
	lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
	lbResp = oShell.Run(lsCadena,1,.T.)

	IF lbResp = 1
		WAIT WINDOW "NO SE PUDO SINCRONIZAR" TIMEOUT 1
	ELSE
	*----------->carga de stock
        IF lcServicioAux = .F.
            agregarStock(lcCodArt)

            lsTable =	["]+'kardex'+["] 
            lsCode	=	["]+ALLTRIM(lcCodArt)+["]
            lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
            lbResp = oShell.Run(lsCadena,1,.T.)

            IF lbResp = 1
                WAIT WINDOW "NO SE SINCRONIZO NINGUN VALOR DE STOCK" TIMEOUT 1
            ENDIF
        ENDIF
	*<-----------fin de carga de stock

		WAIT WINDOW "CONSULTANDO ARTÍCULOS HIJOS" TIMEOUT 1
		SELECT ;
			codart ;
		FROM ;
			articulos_hijos ;
		WHERE ;
			ALLTRIM(articulo) == ALLTRIM(lcCodArt) ;
			AND idexterno == 0 ;
		INTO CURSOR qChild
		
		*!* MESSAGEBOX(RECCOUNT('qChild'))
		IF RECCOUNT('qChild') > 0
			SELECT qChild
			SCAN
				IF oStockRr.enviarVFP('articulos_hijos', ALLTRIM(qChild.codart), ALLTRIM(lcPahtDBCom))
					*----------->carga de stock
					agregarStock(qChild.codart)

                    lsTable =	["]+'kardex'+["] 
                    lsCode	=	["]+ALLTRIM(qChild.codart)+["]
                    lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
                    lbResp = oShell.Run(lsCadena,1,.T.)

                    IF lbResp = 1
						WAIT WINDOW "NO SE SINCRONIZO NINGUN VALOR DE STOCK DEL PRODUCTO HIJO" TIMEOUT 1
					ENDIF
				*<-----------fin de carga de stock
				ENDIF
			ENDSCAN
		ENDIF && RECCOUNT('qChild') > 0

		WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
	ENDIF

	RETURN .T.
ENDFUNC

FUNCTION agregarStock(lcCodart)
	USE IN (SELECT("qValidaCat"))
	USE IN (SELECT("qDatos"))
	lbResult = .F.

	CREATE CURSOR qDatos( ;
		codalmacen varchar(10), ;
		codart varchar(25), ;
		tipinv varchar(5), ;
		cantidad numeric(20, 8), ;
		costo_u numeric(20, 8);
	)

	INSERT INTO qDatos ;
	SELECT ;
		T.CODALMACEN, ;
		T.CODART, ;
		T.TIPINV, ;
		(IIF(T.TIPINV = "E",1,-1)*T.CANTIDAD) AS CANTIDAD, ;
		T.costo_u ;
	FROM ;
		TRANINV T ;
		INNER JOIN ARTICULOS A ON T.CODART = A.CODART;
		INNER JOIN ALMACENES AL ON T.CODALMACEN = AL.CODALMACEN;
	WHERE (A.ECOMMERCE = .T. OR A.MAKI = .T.);
		AND ALLTRIM(A.CODART) == ALLTRIM(lcCodart);
		AND A.IDEXTERNO > 0 ;
		AND AL.SYNC = .T.

	SELECT qDatos
	IF (USED("qDatos") AND RECCOUNT("qDatos") > 0)
		SELECT ;
			T.CODALMACEN, ;
			T.CODART, ;
			SUM(T.CANTIDAD) AS CANTIDAD, ;
			SUM(T.CANTIDAD * T.COSTO_U) AS COSTO, ;
			000000000000.0000000000 AS COSTO_U ;
		FROM ;
			qDatos T ;
		GROUP BY ;
			T.CODALMACEN, ;
			T.CODART ;
		INTO CURSOR CURSTOCKXBODCOS READWRITE

		REPLACE COSTO_U WITH COSTO / CANTIDAD FOR CANTIDAD>0
		
		SELECT ;
			SYS(2015) AS CODIGO, ;
			C.CODART, ;
			C.CODALMACEN, ;
			C.CANTIDAD, ;
			C.COSTO_U,DATE() AS FECHA, ;
			.F. AS SYNC ;
		FROM ;
			CURSTOCKXBODCOS C ;
		WHERE ;
			C.CANTIDAD > 0 ;
		INTO CURSOR CURTEMPSTOCK READWRITE
		*--BROWSE
		
		SELECT CURTEMPSTOCK
		SCAN
			SELECT CODART FROM STOCK WHERE ALLTRIM(CODART) == ALLTRIM(CURTEMPSTOCK.CODART) AND ORIGEN = 'VALIDADOR' AND CODALMACEN = CURTEMPSTOCK.CODALMACEN INTO CURSOR CURVALIDA

			IF TRIM(CURVALIDA.CODART) == "" 
				SELECT (SUM(IIF(TIPOPERACION = 1,EXISTE_ACT,0)) - SUM(IIF(TIPOPERACION = 0,EXISTE_ACT,0))) AS EXISTE_P FROM STOCK WHERE ALLTRIM(CODART) == ALLTRIM(CURTEMPSTOCK.CODART) AND CODALMACEN = CURTEMPSTOCK.CODALMACEN INTO CURSOR CUREXISTE
				LCOP = 1
				LCTIP = 'EI'
				LCCANT = CURTEMPSTOCK.CANTIDAD - CUREXISTE.EXISTE_P
				IF LCCANT<>0
					IF LCCANT < 0
						LCOP = 0
						LCTIP = 'SI'
						LCCANT = LCCANT * -1
					ENDIF
					SELECT IDEXTERNO AS ID FROM TIPOINV WHERE TIPINV = LCTIP INTO CURSOR CURINV
					
					INSERT INTO STOCK( ;
						CODART, ;
						CODALMACEN, ;
						ORIGEN, ;
						UBICACION_ALM, ;
						EXISTE_ACT, ; 
						EXISTE_ACT2, ;
						TIPINV, ;
						TIPOPERACION, ;
						SYNC) ;
					VALUES ( ;
						CURTEMPSTOCK.CODART, ;
						CURTEMPSTOCK.CODALMACEN, ;
						"VALIDADOR", ;
						"ALMACEN", ;
						LCCANT, ;
						CURTEMPSTOCK.COSTO_U, ;
						CURINV.ID, ;
						LCOP, ;
						CURTEMPSTOCK.SYNC ;
					)
					
				ENDIF
			ENDIF
		ENDSCAN
		lbResult = .T.
	ELSE
		WAIT WINDOW "NO EXISTEN DATOS HISTORICOS EN STOCK" TIMEOUT 1
	ENDIF

	RETURN lbResult
ENDFUNC