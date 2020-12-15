*!* VARIABLE OBLIGATORIA
lcExpresion	= .T.

LOCAL lcNombre, lcCodUsr, lcNomUsr, lcFormu, lcPathDB, lcDirPRG, lcPahtDBCom
lcPathDB    = oApp.dbc
lcPahtDBCom = oApp.RUTA
lcDirPRG	= "C:\Fenix\AdsFenix\DATOS_pro\Datos_add\BR_eCommerce\"
lcCodUsr    = ALLTRIM(oApp.user)
lcNomUsr    = ALLTRIM(oApp.nameuser)
lcForm		= ALLTRIM(UPPER(_SCREEN.ACTIVEFORM.NAME))
lcTime		= ALLTRIM(UPPER(CurValidador.VAL_ORD))
lnUBtn		= _SCREEN.ACTIVEFORM.ubtn
 
*!* MESSAGEBOX(lnUBtn, "POLICIA - lnUBtn")
*!* MESSAGEBOX(lcTime, "POLICIA - lcTime")
*!* MESSAGEBOX(lcPahtDBCom, "POLICIA - COMERCIAL")
*!* MESSAGEBOX(lcForm, "POLICIA - FORMULARIO")

*--> CONTROL DEL FORM GENERICO [MANTENIMIENTO]
	TRY
		lcOrigen = ALLTRIM(UPPER(_SCREEN.ACTIVEFORM.Alias1))
		IF lcForm == "MANTENIMIENTO"
			lcForm = lcOrigen
		ELSE && lcForm == "MANTENIMIENTO"
			lcForm = lcOrigen
		ENDIF && lcForm == "MANTENIMIENTO"
	CATCH
	ENDTRY
*<-- CONTROL DEL FORM GENERICO [MANTENIMIENTO]

* WAIT WINDOW "Formulario: " + lcForm NOWAIT
* WAIT WINDOW "Tiempo: " + lcTime NOWAIT

*--> CONFIGURACI�N DE ENTORNO VFP
    SET ENGINEBEHAVIOR 70
    SET EXCLUSIVE OFF
    SET DELETED ON
    SET CENTURY ON
    SET DATE DMY
    SET EXACT OFF
    SET NEAR OFF
*<-- CONFIGURACI�N DE ENTORNO VFP

*--> Si la propiedad es _SCREEN.ACTIVEFORM.fbutton
	TRY
		IF _SCREEN.ACTIVEFORM.AuxBoton = 0
			* MESSAGEBOX(TRANSFORM(_SCREEN.ACTIVEFORM.fbutton), 64, "POLICIA 1")
			lnFButton = _SCREEN.ACTIVEFORM.fbutton
		ELSE
			* MESSAGEBOX(TRANSFORM(_SCREEN.ACTIVEFORM.AuxBoton), 64, "POLICIA 2")
			lnFButton = _SCREEN.ACTIVEFORM.AuxBoton
		ENDIF
		* WAIT TRANSFORM(lnFButton) WINDOW 
	CATCH
		lnFButton = _SCREEN.ACTIVEFORM.fbutton
	ENDTRY
*<-- Si la propiedad es _SCREEN.ACTIVEFORM.fbutton

*!* MESSAGEBOX(lnFButton, "POLICIA - lnFButton")
*!* MESSAGEBOX(lcForm, "POLICIA - lnFButton")
 
*!* -->	DECLARACION DEL EJECUTABLE PARA LA INTEGRACION
	oShell = CREATEOBJECT("WScript.Shell")
	lsRuta 	= 	["]+'C:\Fenix\AdsFenix\DATOS_pro\Datos_add\BR_eCommerce\API\InitIntegration.exe'+["]
	lsBD	=	["]+ALLTRIM(lcPahtDBCom)+["]
*!*	<-- DECLARACION DEL EJECUTABLE PARA LA INTEGRACION

TRY
	DO CASE	&& _SCREEN.ACTIVEFORM = "Nombre de Formulario"
		*--> VALIDADORES PARA FORMULARIO DE MOV. CAJAS
			CASE lcForm == "MOVCAJA_C"
				DO CASE	&& Boton Presinado
					CASE lnFButton = 1 && NUEVO
						lcEvent = "NUEVO"
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 1 && NUEVO

					CASE lnFButton = 3 && MODIFICA
						lcEvent = "MODIFICACION"
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 3 && MODIFICA

					CASE lnFButton = 2 && ELIMINA
						lcEvent = "ELIMINACION"
						
					*<-- lnFButton = 2 && ELIMINA

					CASE lnFButton = 4 && GRABAR
						lcEvent = "GRABAR"
						DO CASE && GRABAR Y TIEMPOS
							CASE lcTime == "ANTES"
								
							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								PUBLIC lnResult
								lnResult = _SCREEN.ACTIVEFORM.lbRes
								lcSec = ALLTRIM(tracaja.sec)

								IF lnResult = .T.
									IF lnUBtn = 1	&& Nuevo
										*!*	DECLARACION DEL EJECUTABLE PARA LA INTEGRACION
										lsTable =	["]+'tracaja'+["] 
										lsCode	=	["]+ALLTRIM(lcSec)+["]
										lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
										lbResp = oShell.Run(lsCadena,1,.T.)

										IF lbResp = 0
											WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
										ENDIF
									ENDIF	&& Fin Nuevo
								ELSE && lnResult = .T.
								ENDIF && lnResult = .T.
							*<-- lcTime == "DESPUES"
						OTHERWISE &&  && GRABAR Y TIEMPOS
						ENDCASE && GRABAR Y TIEMPOS
					*<-- lnFButton = 4 && GRABAR
			ENDCASE && Boton Presinado
			*<-- lcForm == "MOVCAJA_C"
		*<-- VALIDADORES PARA FORMULARIO DE MOV. CAJAS

		*--> VALIDADORES PARA FORMULARIO DE MOV. BANCOS
			CASE lcForm == "MOVBANCO_C"
				DO CASE	&& Boton Presinado
					CASE lnFButton = 1 && NUEVO
						lcEvent = "NUEVO"
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 1 && NUEVO

					CASE lnFButton = 3 && MODIFICA
						lcEvent = "MODIFICACION"
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 3 && MODIFICA

					CASE lnFButton = 2 && ELIMINA
						lcEvent = "ELIMINACION"
						
					*<-- lnFButton = 2 && ELIMINA

					CASE lnFButton = 4 && GRABAR
						lcEvent = "GRABAR"
						DO CASE && GRABAR Y TIEMPOS
							CASE lcTime == "ANTES"
								
							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								PUBLIC lnResult
								lnResult = _SCREEN.ACTIVEFORM.lbRes
								lcSec = ALLTRIM(trabanco.sec)

								IF lnResult = .T.
									IF lnUBtn = 1 && Nuevo
									ENDIF	&& Fin Nuevo
									
									*!*	DECLARACION DEL EJECUTABLE PARA LA INTEGRACION
									lsTable =	["]+'trabanco'+["] 
									lsCode	=	["]+ALLTRIM(lcSec)+["]
									lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
									lbResp = oShell.Run(lsCadena,1,.T.)
										
									IF lbResp = 0
										WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
									ENDIF
								ELSE && lnResult = .T.
								ENDIF && lnResult = .T.
							*<-- lcTime == "DESPUES"
						OTHERWISE &&  && GRABAR Y TIEMPOS
						ENDCASE && GRABAR Y TIEMPOS
					*<-- lnFButton = 4 && GRABAR
				ENDCASE && Boton Presinado	
			*<-- lcForm == "MOVBANCO_C"
		*<-- VALIDADORES PARA FORMULARIO DE MOV. BANCOS

		*--> VALIDADORES PARA FORMULARIO DE FACTURACION Y NC DE VENTAS
			CASE lcForm == "DEVOLFAC_C" OR lcForm == "DEVOLFAC_C2"
				lcNumfac	= ALLTRIM(facturas.numfac)
				DO CASE	&& Que Boton Presiono
					CASE lnFButton = 1 && NUEVO
						* WAIT WINDOW  lcForm + " - Btn NUEVO - " + lcTime NOWAIT
					*<-- lnFButton = 1 && NUEVO

					CASE lnFButton = 3 && MODIFICA
						* WAIT WINDOW  lcForm + " - Btn Modifica - " + lcTime NOWAIT
						DO CASE &&MODIFICA Y TIEMPOS
							CASE lcTime == "ANTES"
							*<-- lcTime == "ANTES"
							
							CASE lcTime == "DESPUES"
							*<-- lcTime == "DESPUES"
								
						OTHERWISE && MODIFICA Y TIEMPOS
						ENDCASE && MODIFICA Y TIEMPOS
					*<-- lnFButton = 3 && MODIFICA
						
					CASE lnFButton = 2 && ELIMINA
						lcNumfacAux	= ALLTRIM(facturas.numfac)
						PUBLIC lcCodAlm, lcInsertStock

						DO CASE &&ELIMINACI�N Y TIEMPOS
							CASE lcTime == "ANTES"
								*lcInsertStock = "INSERT INTO stock(codart, codalmacen, ubicacion_alm, existe_act, existe_act2, tipinv, tipoperacion, fecha) SELECT t.codart as codart, t.codalmacen as codalmacen,	al.nomalmacen as ubicacion_alm,	t.cantidad as existe_act, t.cantidad_d as existe_act2, (SELECT idexterno FROM	tipoinv	WHERE tipinv == 'EI')  as tipinv, IIF(t.tipinv = 'E', 1, 0) as tipoperacion, t.fecha AS fecha FROM traninv t INNER JOIN almacenes al ON (t.codalmacen = al.codalmacen) INNER JOIN articulos ar ON (t.codart = ar.codart) WHERE ALLTRIM(numdoc) == ALLTRIM(lcNumfacAux) AND (ar.ecommerce = .T. OR ar.maki = .T.)"
								SELECT ;
									t.unico AS unico, ;
									t.codart as codart, ;
									t.codalmacen as codalmacen,	;
									ALLTRIM(lcNumfacAux), ;
									t.cantidad as existe_act, ;
									t.cantidad_d as existe_act2, ;
									(SELECT idexterno FROM	tipoinv	WHERE tipinv == 'ED') as tipinv, ;
									1 as tipoperacion, ;
									t.fecha AS fecha ;
								FROM ;
									traninv t ;
									INNER JOIN almacenes al ON (t.codalmacen = al.codalmacen) ;
									INNER JOIN articulos ar ON (t.codart = ar.codart) ;
								WHERE ;
									ALLTRIM(numdoc) == ALLTRIM(lcNumfacAux)  ;
									AND (ar.ecommerce = .T. OR ar.maki = .T.) ;
									AND al.sync = .T. ;
								INTO CURSOR qTraninv READWRITE 
											
								SELECT qTraninv 
								SCAN 
									SELECT ;
										codart, ;
										ubicacion_alm ;
									FROM ;
										stock ;
									WHERE ;
										ubicacion_alm == ALLTRIM(qTraninv.unico) ;
										AND NOT DELETED() ;
									INTO CURSOR qStock READWRITE

									IF RECCOUNT("qStock") = 0 
										INSERT INTO stock(;
											codart, ;
											codalmacen, ;
											origen, ;
											existe_act, ;
											existe_act2, ;
											tipinv, ;
											tipoperacion, ;
											fecha) ;
										VALUES (;
											qTraninv.codart, ;
											qTraninv.codalmacen, ;
											qTraninv.unico, ;
											qTraninv.existe_act, ;
											qTraninv.existe_act2, ;
											qTraninv.tipinv, ;
											qTraninv.tipoperacion, ;
											qTraninv.fecha;
										) 
									ENDIF 
								ENDSCAN 
								
								SELECT codalmacen FROM tranfac WHERE ALLTRIM(numfac) == ALLTRIM(lcNumfacAux) GROUP BY codalmacen INTO CURSOR qCodAlm
								SELECT qCodAlm

								lcCodAlm = qCodAlm.codalmacen
							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								*!*	DECLARACION DEL EJECUTABLE PARA LA INTEGRACION
								lsTable =	["]+'kardexAlmacen'+["] 
								lsCode	=	["]+ALLTRIM(lcCodAlm)+["]
								lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
								lbResp = oShell.Run(lsCadena,1,.T.)
									
								IF lbResp = 0
									WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
								ENDIF
							*<-- lcTime == "DESPUES"	
						OTHERWISE && ELIMINACI�N Y TIEMPOS
						ENDCASE && ELIMINACI�N Y TIEMPOS
					*<-- lnFButton = 2 && ELIMINA

					CASE lnFButton = 4	&&GRABAR
						PUBLIC lnResult
						lnResult	= _SCREEN.ACTIVEFORM.lbRes
						SELECT idexterno FROM tipoinv WHERE tipinv == 'SA' INTO CURSOR qTipinvSal
						SELECT idexterno FROM tipoinv WHERE tipinv == 'ED' INTO CURSOR qTipinvEnt
						*!* lcNumfac	= ALLTRIM(facturas.numfac)

						DO CASE	&& Antes y Despues
							CASE lcTime == "ANTES"
								*!*	CURSOR ANTES DE REGISTRAR O ELIMINAR LINEAS
								SELECT ;
									t.unico AS unico, ;
									t.codart as codart, ;
									t.numdoc as numdoc, ;
									t.codalmacen as codalmacen, ;
									t.cantidad as existe_act ;
								FROM ;
									traninv t ;
									INNER JOIN almacenes al ON t.codalmacen = al.codalmacen;
									INNER JOIN articulos ar ON ar.codart = t.codart;
								WHERE ;
									ALLTRIM(numdoc) == ALLTRIM(lcNumfac) ;
									AND NOT DELETED() ; 
									AND al.sync = .T. ;
								INTO CURSOR qTraninvAnt READWRITE 

							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								IF lnResult = .T.
										&& Creamos un cursor para atrapar los datos relacionados en TRANFAC
										SELECT codart FROM tranfac WHERE numfac = lcNumfac INTO CURSOR qtranfac
										SELECT qtranfac

										&&Reemplazamos ecommerce a true para poder hacer pruebas
										&& Esto debera quitarse al colocar en produccion con el incremento desde Fenix
										* REPLACE ecommerce WITH .T. FOR codart = qtranfac.codart IN articulos
										* TABLEUPDATE(.T., .T., "articulos")

										IF lnUBtn = 1 && Nuevo registro
											INSERT INTO facturas_extras (numfac) VALUES (ALLTRIM(lcNumfac))	
											
											SELECT ;
												t.unico AS unico, ;
												t.codart as codart, ;
												t.numdoc as numdoc, ;
												t.codalmacen as codalmacen, ;
												ALLTRIM(lcNumfac),	;
												t.cantidad as existe_act, ;
												t.cantidad_d as existe_act2, ;
												qTipinvSal.idexterno  as tipinv, ;
												0 as tipoperacion, ;
												t.fecha AS fecha ;
											FROM ;
												traninv t ;
												INNER JOIN almacenes al ON (t.codalmacen = al.codalmacen) ;
												INNER JOIN articulos ar ON (t.codart = ar.codart) ;
											WHERE ;
												ALLTRIM(numdoc) == ALLTRIM(lcNumfac) ;
												AND (ar.ecommerce = .T. OR ar.maki = .T.) ;
												AND al.sync = .T. ;
											INTO CURSOR qTraninv READWRITE 

											SELECT qTraninv 
											SCAN 
												SELECT ;
													codart, ;
													origen ;
												FROM ;
													stock ;
												WHERE ;
													origen == ALLTRIM(qTraninv.unico) ;
													AND NOT DELETED() ;
												INTO CURSOR qStock READWRITE 
												

												IF RECCOUNT("qStock") = 0 
													INSERT INTO stock(;
														codart, ;
														codalmacen, ;
														origen, ;
														existe_act, ;
														existe_act2, ;
														tipinv, ;
														tipoperacion, ;
														fecha ;
													) ;
													VALUES (;
														qTraninv.codart, ;
														qTraninv.codalmacen, ;
														qTraninv.unico, ;
														qTraninv.existe_act, ;
														qTraninv.existe_act2, ;
														qTraninv.tipinv, ;
														qTraninv.tipoperacion, ;
														qTraninv.fecha;
													) 
												ENDIF 
											ENDSCAN 
											* INSERT INTO stock(codart, codalmacen, ubicacion_alm, existe_act, existe_act2, tipinv, tipoperacion, fecha) SELECT t.codart as codart, t.codalmacen as codalmacen,	ALLTRIM(lcNumfacAux),	t.cantidad as existe_act, t.cantidad_d as existe_act2, (SELECT idexterno FROM	tipoinv	WHERE tipinv == 'SA')  as tipinv, IIF(t.tipinv = 'E', 1, 0) as tipoperacion, t.fecha AS fecha FROM traninv t INNER JOIN almacenes al ON (t.codalmacen = al.codalmacen) INNER JOIN articulos ar ON (t.codart = ar.codart) WHERE ALLTRIM(numdoc) == ALLTRIM(lcNumfacAux) AND (ar.ecommerce = .T. OR ar.maki = .T.)

											SELECT codalmacen FROM tranfac WHERE ALLTRIM(numfac) == ALLTRIM(lcNumfac) GROUP BY codalmacen INTO CURSOR qCodAlmDev
											SELECT qCodAlmDev
											
											*!*	DECLARACION DEL EJECUTABLE PARA LA INTEGRACION
											lsTable =	["]+'kardexAlmacen'+["] 
											lsCode	=	["]+ALLTRIM(qCodAlmDev.codalmacen)+["]
											lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
											lbResp = oShell.Run(lsCadena,1,.T.)
												
											IF lbResp = 1
												*!* MESSAGEBOX(oStockDev.sms, "RESULTADO")
												recalcularStock(ALLTRIM(qCodAlmDev.codalmacen), ALLTRIM(lcPahtDBCom))
											ELSE
												WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
											ENDIF
										ENDIF && Fin Nuevo registro

										IF lnUBtn = 3 && Actualiza registro
											UPDATE facturas_extras SET update = .T., sync = .F. WHERE numfac = (ALLTRIM(lcNumfac))

											SELECT qTraninvAnt
											
											*!*	CURSOR DESPUES DE REGISTRAR O ELIMINAR LINEAS
											SELECT ;
												t.unico AS unico, ;
												t.codart as codart, ;
												t.numdoc as numdoc, ;
												t.codalmacen as codalmacen, ;
												t.cantidad as existe_act ;
											FROM ;
												traninv t ;
												INNER JOIN almacenes al ON t.codalmacen = al.codalmacen ;
												INNER JOIN articulos ar ON ar.codart = t.codart ;
											WHERE ;
												ALLTRIM(numdoc) == ALLTRIM(lcNumfac) ;
												AND NOT DELETED() ;
												AND al.sync = .T. ;
											INTO CURSOR qTraninvDes READWRITE 

											CREATE CURSOR qResultante( ;
												unico varchar(10), ;
												codart varchar(25), ;
												numdoc varchar(10), ;
												codalmacen varchar(10), ;
												existe_act numeric(20,8), ;
												tipinv numeric(10,0), ;
												tipoperacion int(1) ;
											)

											*!* OBTENER REGISTROS DIFERENTES
											INSERT INTO qResultante ;
											SELECT ;
												qta.unico AS unico, ;
												qta.codart as codart, ;
												qta.numdoc as numdoc, ;
												qta.codalmacen as codalmacen, ;
												qta.existe_act as existe_act, ;
												qTipinvEnt.idexterno as tipinv, ;
												1 as tipoperacion ;
											FROM ;
												qTraninvAnt qta ;
												LEFT JOIN qTraninvDes qtd ON qta.unico = qtd.unico ;
											WHERE ;
												qtd.unico is null

											*!* OBTENER REGISTROS DIFERENTES
											INSERT INTO qResultante ;
											SELECT ;
												qtd.unico AS unico, ;
												qtd.codart as codart, ;
												qtd.numdoc as numdoc, ;
												qtd.codalmacen as codalmacen, ;
												qtd.existe_act as existe_act, ;
												qTipinvSal.idexterno as tipinv, ;
												0 as tipoperacion ;
											FROM ;
												qTraninvDes qtd ;
												LEFT JOIN qTraninvAnt qta ON qtd.unico = qta.unico ;
											WHERE ;
												qta.unico is null

											*!* CURSOR CON LOS DATOS A REGISTRAR
											SELECT ;
												qres.codart, ;
												qres.codalmacen, ;
												qres.unico, ;
												qres.existe_act, ;
												qres.tipinv, ;
												qres.tipoperacion ;
											FROM ;
												qResultante qres ;
												INNER JOIN articulos ar ON qres.codart = ar.codart ;
											WHERE ;
												ar.ecommerce = .T. ;
												OR ar.maki = .T. ;
											INTO CURSOR qStock READWRITE 

											*!* REGISTRO A TABLA STOCK
											INSERT INTO stock( ;
												codart, ;
												codalmacen, ;
												origen, ;
												existe_act, ;
												tipinv, ;
												tipoperacion, ;
												sync) ;
											SELECT ;
												qStock.codart AS codart, ;
												qStock.codalmacen AS codalmacen, ;
												qStock.unico AS origen, ;
												qStock.existe_act AS existe_act, ;
												qStock.tipinv AS tipinv, ;
												qStock.tipoperacion AS tipoperacion, ;
												.F. AS sync ;
											FROM ;
												qStock

											*!* LIBRERIA C SHARP
											SELECT codalmacen FROM tranfac WHERE ALLTRIM(numfac) == ALLTRIM(lcNumfac) GROUP BY codalmacen INTO CURSOR qCodAlmDev
											SELECT qCodAlmDev

											*!*	DECLARACION DEL EJECUTABLE PARA LA INTEGRACION
											lsTable =	["]+'kardexAlmacen'+["] 
											lsCode	=	["]+ALLTRIM(qCodAlmDev.codalmacen)+["]
											lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
											lbResp = oShell.Run(lsCadena,1,.T.)

											IF lbResp = 1
												recalcularStock(ALLTRIM(qCodAlmDev.codalmacen), ALLTRIM(lcPahtDBCom))
												*!* WAIT WINDOW oStockDev.sms TIMEOUT 1
											ELSE
												WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
											ENDIF
										ENDIF && Fin Actualiza registro	

										IF lnUBtn = -1 && Devolución
											* INSERT INTO stock(codart, codalmacen, ubicacion_alm, existe_act, existe_act2, tipinv, tipoperacion, fecha) SELECT t.codart as codart, t.codalmacen as codalmacen,	al.nomalmacen as ubicacion_alm,	t.cantidad as existe_act, t.cantidad_d as existe_act2, (SELECT idexterno FROM	tipoinv	WHERE tipinv == 'ED')  as tipinv, IIF(t.tipinv = 'E', 1, 0) as tipoperacion, t.fecha AS fecha FROM traninv t INNER JOIN almacenes al ON (t.codalmacen = al.codalmacen) INNER JOIN articulos ar ON (t.codart = ar.codart) WHERE ALLTRIM(numdoc) == ALLTRIM(lcNumfacAux) AND (ar.ecommerce = .T. OR ar.maki = .T.)
											SELECT ;
												t.unico AS unico, ;
												t.codart as codart, ;
												t.codalmacen as codalmacen,	;
												ALLTRIM(lcNumfac),	;
												t.cantidad as existe_act, ;
												t.cantidad_d as existe_act2, ;
												qTipinvEnt.idexterno as tipinv, ;
												1 as tipoperacion, ;
												t.fecha AS fecha ;
											FROM ;
												traninv t ;
												INNER JOIN almacenes al ON (t.codalmacen = al.codalmacen) ;
												INNER JOIN articulos ar ON (t.codart = ar.codart) ;
											WHERE ;
												ALLTRIM(numdoc) == ALLTRIM(lcNumfac) ;
												AND (ar.ecommerce = .T. OR ar.maki = .T.) ;
												AND al.sync = .T. ;
											INTO CURSOR qTraninv READWRITE

											SELECT qTraninv 
											SCAN 
												SELECT ;
													codart, ;
													ubicacion_alm ;
												FROM ;
													stock ;
												WHERE ;
													ubicacion_alm == ALLTRIM(qTraninv.unico) ;
													AND NOT DELETED() ;
												INTO CURSOR qStock READWRITE 
												*!* MESSAGEBOX("POLICIA 1", RECCOUNT("qStock"))

												IF RECCOUNT("qStock") = 0 
													INSERT INTO stock( ;
														codart, ;
														codalmacen, ;
														origen, ;
														existe_act, ;
														existe_act2, ;
														tipinv, ;
														tipoperacion, ;
														fecha);
													VALUES ( ;
														qTraninv.codart, ;
														qTraninv.codalmacen, ;
														qTraninv.unico, ;
														qTraninv.existe_act, ;
														qTraninv.existe_act2, ;
														qTraninv.tipinv, ;
														qTraninv.tipoperacion, ;
														qTraninv.fecha;
													) 
												ENDIF 
											ENDSCAN
											
											SELECT codalmacen FROM tranfac WHERE ALLTRIM(numfac) == ALLTRIM(lcNumfac) GROUP BY codalmacen INTO CURSOR qCodAlmDev
											SELECT qCodAlmDev

											lsTable =	["]+'kardexAlmacen'+["] 
											lsCode	=	["]+ALLTRIM(qCodAlmDev.codalmacen)+["]
											lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
											lbResp = oShell.Run(lsCadena,1,.T.)
												
											IF lbResp = 1
												recalcularStock(ALLTRIM(qCodAlmDev.codalmacen), ALLTRIM(lcPahtDBCom))
											ELSE
												WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
											ENDIF	
										ENDIF && Fin Devolución	
									ELSE &&lnResult = .T.
									ENDIF &&lnResult = .T.
							*<-- lcTime == "DESPUES"

							OTHERWISE && Otro caso
						ENDCASE && Fin Do Case

						&& STORE SPACE(1) TO _SCREEN.ACTIVEFORM.lbRes, lnResult, _SCREEN.ACTIVEFORM.varvalida1, lcTable, lcDirTable, lcTdb 
					*<-- && lnFButton = 4	&&GRABAR
				ENDCASE && Boton Presinado		
			*<-- lcForm == "DEVOLFAC_C"
		*<-- VALIDADORES PARA FORMULARIO DE FACTURACION Y NC DE VENTAS

		*--> VALIDADORES PARA FORMULARIO DE COMPRAS Y NC
			CASE lcForm == "COMPRAS_C"
				lcNumfac	= ALLTRIM(encprq.numreq)

				DO CASE	&& Que Boton Presiono
					CASE lnFButton = 1 && NUEVO
						* WAIT WINDOW  lcForm + " - Btn Nuevo - " + lcTime &&NOWAIT
					&& lnFButton = 1 && NUEVO

					CASE lnFButton = 3 && MODIFICA
						DO CASE &&MODIFICA Y TIEMPOS
							CASE lcTime == "ANTES"
								* WAIT WINDOW  lcForm + " - Btn Modifica - " + lcTime &&NOWAIT
							&& lcTime == "ANTES"
							
							CASE lcTime == "DESPUES"
								* WAIT WINDOW  lcForm + " - Btn Modifica - " + lcTime &&NOWAIT
							&& lcTime == "DESPUES"
								
						OTHERWISE && MODIFICA Y TIEMPOS
						ENDCASE && MODIFICA Y TIEMPOS
					&& lnFButton = 3 && MODIFICA

					CASE lnFButton = 2 && ELIMINA
						PUBLIC lcCodAlm

						DO CASE &&ELIMINACI�N Y TIEMPOS
							CASE lcTime == "ANTES"
								* WAIT WINDOW  lcForm + " - Btn Elimina - " + lcTime &&NOWAIT
								SELECT ;
									t.unico AS unico, ;
									t.codart as codart, ;
									t.codalmacen as codalmacen,	;
									ALLTRIM(lcNumfac),	;
									t.cantidad as existe_act, ;
									t.cantidad_d as existe_act2, ;
									(SELECT idexterno FROM	tipoinv	WHERE tipinv == 'SD')  as tipinv, ;
									0 as tipoperacion, ;
									t.fecha AS fecha ;
								FROM ;
									traninv t ;
									INNER JOIN almacenes al ON (t.codalmacen = al.codalmacen) ;
									INNER JOIN articulos ar ON (t.codart = ar.codart) ;
								WHERE ;
									ALLTRIM(numdoc) == ALLTRIM(lcNumfac) ;
									AND (ar.ecommerce = .T. OR ar.maki = .T.) ;
									AND al.sync = .T. ;
								INTO CURSOR qTraninv READWRITE 
											
								SELECT qTraninv 
								SCAN 
									SELECT ;
										codart, ;
										ubicacion_alm ;
									FROM ;
										stock ;
									WHERE ;
										ubicacion_alm == ALLTRIM(qTraninv.unico) ;
										AND NOT DELETED() ;
									INTO CURSOR qStock READWRITE 

									IF RECCOUNT("qStock") = 0 
										INSERT INTO stock(;
											codart, ;
											codalmacen, ;
											origen, ;
											existe_act, ;
											existe_act2, ;
											tipinv, ;
											tipoperacion, ;
											fecha) ;
										VALUES ( ;
											qTraninv.codart, ;
											qTraninv.codalmacen, ;
											qTraninv.unico, ;
											qTraninv.existe_act, ;
											qTraninv.existe_act2, ;
											qTraninv.tipinv, ;
											qTraninv.tipoperacion, ;
											qTraninv.fecha ;
										) 
									ENDIF 
								ENDSCAN

								SELECT codalmacen FROM tranfac WHERE ALLTRIM(numfac) == ALLTRIM(lcNumfac) GROUP BY codalmacen INTO CURSOR qCodAlm
								SELECT qCodAlm

								lcCodAlm = qCodAlm.codalmacen

							&& lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								lsTable =	["]+'kardexAlmacen'+["] 
								lsCode	=	["]+ALLTRIM(lcCodAlm)+["]
								lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
								lbResp = oShell.Run(lsCadena,1,.T.)
									
								IF lbResp = 1
									recalcularStock(ALLTRIM(lcCodAlm), ALLTRIM(lcPahtDBCom))
								ELSE
									WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
								ENDIF
							&& lcTime == "DESPUES"
						OTHERWISE && ELIMINACI�N Y TIEMPOS
						ENDCASE && ELIMINACI�N Y TIEMPOS
					&& lnFButton = 2 && ELIMINA

					CASE lnFButton = 4	&&GRABAR
						PUBLIC lnResult
						lnResult	= _SCREEN.ACTIVEFORM.lbRes
						SELECT idexterno FROM tipoinv WHERE tipinv == 'SD' INTO CURSOR qTipinvSalC
						SELECT idexterno FROM tipoinv WHERE tipinv == 'EN' INTO CURSOR qTipinvEntC

						DO CASE	&& Antes y Despues
							CASE lcTime == "ANTES"
								SELECT ;
									t.unico AS unico, ;
									t.codart as codart, ;
									t.numdoc as numdoc, ;
									t.codalmacen as codalmacen, ;
									t.cantidad as existe_act ;
								FROM ;
									traninv t ;
									INNER JOIN almacenes al ON t.codalmacen = al.codalmacen ;
								WHERE ;
									ALLTRIM(numdoc) == ALLTRIM(lcNumfac) ;
									AND NOT DELETED() ;
									AND al.sync = .T. ;
								INTO CURSOR qTraninvAntC READWRITE 

							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								IF lnResult = .T.
										IF lnUBtn = 1 && Nuevo registro
											* INSERT INTO stock(codart, codalmacen, ubicacion_alm, existe_act, existe_act2, tipinv, tipoperacion, fecha) SELECT t.codart as codart, t.codalmacen as codalmacen,	al.nomalmacen as ubicacion_alm,	t.cantidad as existe_act, t.cantidad_d as existe_act2, (SELECT idexterno FROM	tipoinv	WHERE tipinv == 'EN')  as tipinv, IIF(t.tipinv = 'E', 1, 0) as tipoperacion, t.fecha AS fecha FROM traninv t INNER JOIN almacenes al ON (t.codalmacen = al.codalmacen) INNER JOIN articulos ar ON (t.codart = ar.codart) WHERE ALLTRIM(numdoc) == ALLTRIM(lcNumfac) AND (ar.ecommerce = .T. OR ar.maki = .T.)
											SELECT ;
												t.unico AS unico, ;
												t.codart as codart, ;
												t.codalmacen as codalmacen,	;
												ALLTRIM(lcNumfac),	;
												t.cantidad as existe_act, ;
												t.cantidad_d as existe_act2, ;
												(SELECT idexterno FROM	tipoinv	WHERE tipinv == 'EN')  as tipinv, ;
												1 as tipoperacion, ;
												t.fecha AS fecha ;
											FROM ;
												traninv t ;
												INNER JOIN almacenes al ON (t.codalmacen = al.codalmacen) ;
												INNER JOIN articulos ar ON (t.codart = ar.codart) ;
											WHERE ;
												ALLTRIM(numdoc) == ALLTRIM(lcNumfac) ;
												AND (ar.ecommerce = .T. OR ar.maki = .T.) ;
												AND al.sync = .T. ;
											INTO CURSOR qTraninv READWRITE 
											
											SELECT qTraninv 
											SCAN 
												SELECT ;
													codart, ;
													ubicacion_alm ;
												FROM ;
													stock ;
												WHERE ;
													ubicacion_alm == ALLTRIM(qTraninv.unico) ;
													AND NOT DELETED() ;
													INTO CURSOR qStock READWRITE

												IF RECCOUNT("qStock") = 0 
													INSERT INTO stock( ;
														codart, ;
														codalmacen, ;
														origen, ;
														existe_act, ;
														existe_act2, ;
														tipinv, ;
														tipoperacion, ;
														fecha) ;
													VALUES ( ;
														qTraninv.codart, ;
														qTraninv.codalmacen, ;
														qTraninv.unico, ;
														qTraninv.existe_act, ;
														qTraninv.existe_act2, ;
														qTraninv.tipinv, ;
														qTraninv.tipoperacion, ;
														qTraninv.fecha;
													) 
												ENDIF 
											ENDSCAN
											
											SELECT codalmacen FROM renprq WHERE ALLTRIM(numreq) == ALLTRIM(lcNumfac) GROUP BY codalmacen INTO CURSOR qCodAlmRp
											SELECT qCodAlmRp

											lsTable =	["]+'kardexAlmacen'+["] 
											lsCode	=	["]+ALLTRIM(qCodAlmRp.codalmacen)+["]
											lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
											lbResp = oShell.Run(lsCadena,1,.T.)
												
											IF lbResp = 1
												recalcularStock(ALLTRIM(qCodAlmRp.codalmacen), ALLTRIM(lcPahtDBCom))
											ELSE
												WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
											ENDIF
										ENDIF && Fin Nuevo registro

										IF lnUBtn = 3 && Actualiza registro
											SELECT qTraninvAntC

											*!*	CURSOR DESPUES DE REGISTRAR O ELIMINAR LINEAS
											SELECT ;
												t.unico AS unico, ;
												t.codart as codart, ;
												t.numdoc as numdoc, ;
												t.codalmacen as codalmacen, ;
												t.cantidad as existe_act ;
											FROM ;
												traninv t ;
												INNER JOIN almacenes al ON t.codalmacen = al.codalmacen ;
											WHERE ;
												ALLTRIM(numdoc) == ALLTRIM(lcNumfac) ;
												AND NOT DELETED() ;
												AND al.sync = .T. ;
											INTO CURSOR qTraninvDesC READWRITE 

											CREATE CURSOR qResultante( ;
												unico varchar(10), ;
												codart varchar(25), ;
												numdoc varchar(10), ;
												codalmacen varchar(10), ;
												existe_act numeric(20,8), ;
												tipinv numeric(10,0), ;
												tipoperacion int(1) ;
											)

											*!* OBTENER REGISTROS DIFERENTES
											INSERT INTO qResultante ;
											SELECT ;
												qta.unico AS unico, ;
												qta.codart as codart, ;
												qta.numdoc as numdoc, ;
												qta.codalmacen as codalmacen, ;
												qta.existe_act as existe_act, ;
												qTipinvSalC.idexterno as tipinv, ;
												0 as tipoperacion ;
											FROM ;
												qTraninvAntC qta ;
												LEFT JOIN qTraninvDesC qtd ON qta.unico = qtd.unico ;
											WHERE ;
												qtd.unico is null

											*!* OBTENER REGISTROS DIFERENTES
											INSERT INTO qResultante ;
											SELECT ;
												qtd.unico AS unico, ;
												qtd.codart as codart, ;
												qtd.numdoc as numdoc, ;
												qtd.codalmacen as codalmacen, ;
												qtd.existe_act as existe_act, ;
												qTipinvEntC.idexterno as tipinv, ;
												1 as tipoperacion ;
											FROM ;
												qTraninvDesC qtd ;
												LEFT JOIN qTraninvAntc qta ON qtd.unico = qta.unico ;
											WHERE ;
												qta.unico is null

											*!* CURSOR CON LOS DATOS A REGISTRAR
											SELECT ;
												qres.codart, ;
												qres.codalmacen, ;
												qres.unico, ;
												qres.existe_act, ;
												qres.tipinv, ;
												qres.tipoperacion ;
											FROM ;
												qResultante qres ;
												INNER JOIN articulos ar ON qres.codart = ar.codart ;
											WHERE ;
												ar.ecommerce = .T. ;
												OR ar.maki = .T. ;
											INTO CURSOR qStock READWRITE 
											
											*!* REGISTRO A TABLA STOCK
											INSERT INTO stock( ;
												codart, ;
												codalmacen, ;
												origen, ;
												existe_act, ;
												tipinv, ;
												tipoperacion, ;
												sync) ;
											SELECT ;
												qStock.codart AS codart, ;
												qStock.codalmacen AS codalmacen, ;
												qStock.unico AS origen, ;
												qStock.existe_act AS existe_act, ;
												qStock.tipinv AS tipinv, ;
												qStock.tipoperacion AS tipoperacion, ;
												.F. AS sync ;
											FROM ;
												qStock

											*!* LIBRERIA C SHARP
											SELECT codalmacen FROM tranfac WHERE ALLTRIM(numfac) == ALLTRIM(lcNumfac) GROUP BY codalmacen INTO CURSOR qCodAlmDev
											SELECT qCodAlmDev

											lsTable =	["]+'kardexAlmacen'+["] 
											lsCode	=	["]+ALLTRIM(qCodAlmDev.codalmacen)+["]
											lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
											lbResp = oShell.Run(lsCadena,1,.T.)
												
											IF lbResp = 1
												recalcularStock(ALLTRIM(qCodAlmDev.codalmacen), ALLTRIM(lcPahtDBCom))
												*!* WAIT WINDOW oStockDev.sms TIMEOUT 1
											ELSE
												WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
											ENDIF
										ENDIF && Fin Actualiza registro

										IF lnUBtn = -1 && Nota de Crédito
											SELECT ;
												t.unico AS unico, ;
												t.codart as codart, ;
												t.codalmacen as codalmacen,	;
												ALLTRIM(lcNumfac),	;
												t.cantidad as existe_act, ;
												t.cantidad_d as existe_act2, ;
												(SELECT idexterno FROM	tipoinv	WHERE tipinv == 'SD') as tipinv, ;
												0 as tipoperacion, ;
												t.fecha AS fecha ;
											FROM ;
												traninv t ;
												INNER JOIN almacenes al ON (t.codalmacen = al.codalmacen) ;
												INNER JOIN articulos ar ON (t.codart = ar.codart) ;
											WHERE ;
												ALLTRIM(numdoc) == ALLTRIM(lcNumfac) ;
												AND (ar.ecommerce = .T. OR ar.maki = .T.) ;
												AND al.sync = .T. ;
												INTO CURSOR qTraninv READWRITE 
											
											SELECT qTraninv 
											SCAN 
												SELECT ;
													codart, ;
													ubicacion_alm ;
												FROM ;
													stock ;
												WHERE ;
													ubicacion_alm == ALLTRIM(qTraninv.unico) ;
													AND NOT DELETED() ;
												INTO CURSOR qStock READWRITE 

												IF RECCOUNT("qStock") = 0 
													INSERT INTO stock( ;
														codart, ;
														codalmacen, ;
														origen, ;
														existe_act, ;
														existe_act2, ;
														tipinv, ;
														tipoperacion, ;
														fecha) ;
													VALUES ( ;
														qTraninv.codart, ;
														qTraninv.codalmacen, ;
														qTraninv.unico, ;
														qTraninv.existe_act, ;
														qTraninv.existe_act2, ;
														qTraninv.tipinv, ;
														qTraninv.tipoperacion, ;
														qTraninv.fecha;
													) 
												ENDIF 
											ENDSCAN

											SELECT codalmacen FROM tranfac WHERE ALLTRIM(numfac) == ALLTRIM(lcNumfac) GROUP BY codalmacen INTO CURSOR qCodAlmDev
											SELECT qCodAlmDev

											lsTable =	["]+'kardexAlmacen'+["] 
											lsCode	=	["]+ALLTRIM(qCodAlmDev.codalmacen)+["]
											lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
											lbResp = oShell.Run(lsCadena,1,.T.)
												
											IF lbResp = 1
												recalcularStock(ALLTRIM(qCodAlmDev.codalmacen), ALLTRIM(lcPahtDBCom))
											ELSE
												WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
											ENDIF
										ENDIF && Fin Nota de Crédito
		
									ELSE &&lnResult = .T.
									ENDIF &&lnResult = .T.
							*<-- lcTime == "DESPUES"

							OTHERWISE && Otro caso
						ENDCASE && Fin Do Case
						* STORE SPACE(1) TO _SCREEN.ACTIVEFORM.lbRes, lnResult, _SCREEN.ACTIVEFORM.varvalida1, lcMov, lcTable, lcDirTable, lcTdb 
					*<-- && lnFButton = 4	&&GRABAR
				ENDCASE && Boton Presinado			
			&& lcForm == "COMPRAS_C"
		*<-- VALIDADORES PARA FORMULARIO DE COMPRAS Y NC

		*--> VALIDADORES PARA FORMULARIO DE COBROS
			CASE lcForm == "CXC"
				DO CASE && Boton Presinado
					CASE lnFButton = 1 && NUEVO
						lcEvent = "NUEVO"
						IF lcTime == "ANTES"
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 1 && NUEVO

					CASE lnFButton = 3 && MODIFICA
						lcEvent = "MODIFICACION"
						IF lcTime == "ANTES"
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 3 && MODIFICA

					CASE lnFButton = 2 && ELIMINA
						lcEvent = "ELIMINACION"
						* PUBLIC lcCodigo
						DO CASE && ELIMINA Y TIEMPOS
							CASE lcTime == "ANTES"
								* WAIT WINDOW "Bot�n: [" + TRANSFORM(lnFButton) + "]. Formulario: [" + lcForm + "]. Evento: [" + lcEvent + "]. Tiempo: [" + lcTime + "]"
								* WAIT WINDOW "U. Bot�n: [" + TRANSFORM(lnUBtn) + "]. Formulario: [" + lcForm + "]. Evento: [" + lcEvent + "]. Tiempo: [" + lcTime + "]"
							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								* PUBLIC lnResult
								* lnResult = _SCREEN.ACTIVEFORM.lbRes

								* IF lnResult = .T.
								* ELSE && lnResult = .T.
								* ENDIF && lnResult = .T.
							*<-- lcTime == "DESPUES"
							* STORE SPACE(1) TO _SCREEN.ACTIVEFORM.lbRes, lnResult, _SCREEN.ACTIVEFORM.varvalida1, lcCodigo
						OTHERWISE &&  && ELIMINA Y TIEMPOS
						ENDCASE && ELIMINA Y TIEMPOS
					*<-- lnFButton = 2 && ELIMINA

					CASE lnFButton = 4 && GRABAR
						lcEvent = "GRABAR"
						DO CASE && GRABAR Y TIEMPOS
							CASE lcTime == "ANTES"
							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								PUBLIC lnResult
								lnResult = _SCREEN.ACTIVEFORM.lbRes
								IF lnResult = .T.
									
								ELSE && lnResult = .T.
								ENDIF && lnResult = .T.
							*<-- lcTime == "DESPUES"
						OTHERWISE &&  && GRABAR Y TIEMPOS
						ENDCASE && GRABAR Y TIEMPOS
					*<-- lnFButton = 4 && GRABAR
				ENDCASE && Boton Presinado			
			*<-- lcForm == "CXC"
		*<-- VALIDADORES PARA FORMULARIO DE COBROS

		*--> VALIDADORES PARA FORMULARIO DE CTASXP_C
			CASE lcForm == "CTASXP_C"
				DO CASE	
					CASE lnFButton = 1
						* WAIT WINDOW  "Nuevo CTASXP_C"
					CASE lnFButton = 3
						* WAIT WINDOW  "Modifica CTASXP_C"
					CASE lnFButton = 2
						* WAIT WINDOW  "Elimina CTASXP_C"
				ENDCASE && Boton Presinado			
		*<-- VALIDADORES PARA FORMULARIO DE CTASXP_C

		*--> VALIDADORES PARA FORMULARIO DE CLIENTES
			CASE lcForm == "CLIENTES1_C"
				DO CASE	
					CASE lnFButton = 5 && NUEVO
						IF lcTime == "ANTES"
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 5 && NUEVO

					CASE lnFButton = 7 && MODIFICA
						IF lcTime == "ANTES"
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 7 && MODIFICA

					CASE lnFButton = 6 && ELIMINA
						PUBLIC lcCodigo
						DO CASE && ELIMINA Y TIEMPOS
							CASE lcTime == "ANTES"
							CASE lcTime == "DESPUES"
								IF lnResult = .T.
								ELSE && lnResult = .T.
								ENDIF && lnResult = .T.
							*<-- lcTime == "DESPUES"
							&& STORE SPACE(1) TO _SCREEN.ACTIVEFORM.lbRes, lnResult, _SCREEN.ACTIVEFORM.varvalida1, lcCodigo
						OTHERWISE &&  && ELIMINA Y TIEMPOS
						ENDCASE && ELIMINA Y TIEMPOS
					*<-- lnFButton = 6 && ELIMINA

					CASE lnFButton = 8 && GRABAR
						DO CASE && GRABAR Y TIEMPOS
							CASE lcTime == "ANTES"
							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								PUBLIC lnResult
								lnResult = _SCREEN.ACTIVEFORM.lbRes

								IF lnResult = .T.
									lcCodCli = ALLTRIM(clientes.codcli)

									IF lnFButton = 5 && insertar
										
									ENDIF && fin insertar
									IF lnFButton = 7 && modificar
										UPDATE clientes SET sync = .F., update = .T. WHERE codcli = lcCodCli
										TABLEUPDATE(.T., .T., "clientes")
									ENDIF && fin modificar

									lsTable =	["]+'clientes'+["] 
									lsCode	=	["]+ALLTRIM(lcCodCli)+["]
									lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
									lbResp = oShell.Run(lsCadena,1,.T.)
										
									IF lbResp = 0
										WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
									ENDIF	
								ELSE && lnResult = .T.
								ENDIF && lnResult = .T.
							*<-- lcTime == "DESPUES"
						OTHERWISE &&  && GRABAR Y TIEMPOS
						ENDCASE && GRABAR Y TIEMPOS
					*<-- lnFButton = 8 && GRABAR
				ENDCASE && Boton Presinado			
			*<-- lcForm == "CLIENTES1_C"
		*<-- VALIDADORES PARA FORMULARIO DE CLIENTES

		*--> VALIDADORES PARA FORMULARIO DE GRUPO CLIENTES
			CASE ALLTRIM(UPPER(lcForm)) == "GCLIENTES"
				DO CASE	&& Que Boton Presiono
					CASE lnFButton = 1	&&NUEVO
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 1	&&NUEVO

					CASE lnFButton = 2 && ELIMINA
						DO CASE && ELIMINA Y TIEMPOS
							CASE lcTime == "ANTES"
								
							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								
							*<-- lcTime == "DESPUES"
						OTHERWISE &&  && ELIMINA Y TIEMPOS
						ENDCASE && ELIMINA Y TIEMPOS
					*<-- lnFButton = 2 && ELIMINA

					CASE lnFButton = 3 && MODIFICA
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 3 && MODIFICA

					CASE lnFButton = 4 && GRABAR
						DO CASE && GRABAR Y TIEMPOS
							CASE lcTime == "ANTES"
								
							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								PUBLIC lnResult
								lnResult = _SCREEN.ACTIVEFORM.lbRes

								IF lnResult = .T.
									
								ELSE && lnResult = .T.
								ENDIF && lnResult = .T.
							*<-- lcTime == "DESPUES"
						OTHERWISE &&  && GRABAR Y TIEMPOS
						ENDCASE && GRABAR Y TIEMPOS
					*<-- lnFButton = 4 && GRABAR

					CASE lnFButton = 5 && CANCELA
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 5 && CANCELA
				OTHERWISE && Que Boton Presiono
				ENDCASE	&& Que Boton Presiono
			*<-- ALLTRIM(lcOrigen) == "GCLIENTES"
		*<-- VALIDADORES PARA FORMULARIO DE GRUPO CLIENTES

		*--> VALIDADORES PARA FORMULARIO DE PROVEEDORES
			CASE lcForm == "PROVEEDORES1"
				DO CASE	
					CASE _SCREEN.ACTIVEFORM.button= 5
						* WAIT WINDOW  "Nuevo PROVEEDORES1"
					CASE _SCREEN.ACTIVEFORM.button= 7
						* WAIT WINDOW  "Modifica PROVEEDORES1"
					CASE _SCREEN.ACTIVEFORM.button= 6
						* WAIT WINDOW  "Elimina PROVEEDORES1"
				ENDCASE && Boton Presinado			
		*<-- VALIDADORES PARA FORMULARIO DE PROVEEDORES

		*--> VALIDADORES PARA FORMULARIO DE PEDIDOS DE VENTA
			CASE lcForm == "PEDIDOS_C"
				DO CASE && Boton Presinado
					CASE lnFButton = 1 && NUEVO
						lcEvent = "NUEVO"
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 1 && NUEVO

					CASE lnFButton = 3 && MODIFICA
						lcEvent = "MODIFICACION"
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 3 && MODIFICA

					CASE lnFButton = 2 && ELIMINA
						
					*<-- lnFButton = 2 && ELIMINA

					CASE lnFButton = 4 && GRABAR
						lcEvent = "GRABAR"
						DO CASE && GRABAR Y TIEMPOS
							CASE lcTime == "ANTES"

							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								PUBLIC lnResult
								lnResult = _SCREEN.ACTIVEFORM.lbRes

								IF lnResult = .T.
									lcNumPed = ALLTRIM(encped.numfac)

									IF lnUBtn = 1 && Nuevo
									ENDIF && Fin Nuevo
									IF lnUBtn = 3 && Modifica
										REPLACE SYNC WITH .F. FOR numfac = lcNumPed IN encped
										REPLACE UPDATE WITH .T. FOR numfac = lcNumPed IN encped
										TABLEUPDATE(.T., .T., "encped")
									ENDIF && Fin Modifica

									lsTable =	["]+'encped'+["] 
									lsCode	=	["]+ALLTRIM(lcNumPed)+["]
									lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
									lbResp = oShell.Run(lsCadena,1,.T.)
										
									IF lbResp = 0
										MESSAGEBOX(oLib.sms, "PROCESO CONCLUIDO")
									ENDIF	
								ELSE && lnResult = .T.
								ENDIF && lnResult = .T.
							*<-- lcTime == "DESPUES"
						OTHERWISE &&  && GRABAR Y TIEMPOS
						ENDCASE && GRABAR Y TIEMPOS
					*<-- lnFButton = 4 && GRABAR
				ENDCASE && Boton Presinado		
			*<-- lcForm == "PEDIDOS_C"
		*<-- VALIDADORES PARA FORMULARIO DE PEDIDOS DE VENTA

		*--> VALIDADORES PARA FORMULARIO DE PEDIDOS DE COMPRA
			CASE lcForm == "PEDIDOS_P"
				DO CASE	&& Que Boton Presiono
					CASE lnFButton = 1 && NUEVO
						* WAIT WINDOW  lcForm + " - Btn Nuevo - " + lcTime NOWAIT
					*<-- lnFButton = 1 && NUEVO

					CASE lnFButton = 3 && MODIFICA
						* WAIT WINDOW  lcForm + " - Btn Modifica - " + lcTime NOWAIT

						DO CASE &&MODIFICA Y TIEMPOS
							CASE lcTime == "ANTES"
								
							&& lcTime == "ANTES"
							
							CASE lcTime == "DESPUES"
								
							&& lcTime == "DESPUES"
								
						OTHERWISE && MODIFICA Y TIEMPOS
						ENDCASE && MODIFICA Y TIEMPOS
					*<-- lnFButton = 3 && MODIFICA

					CASE lnFButton = 2 && ELIMINA
						DO CASE &&ELIMINACI�N Y TIEMPOS
							CASE lcTime == "ANTES"
								* WAIT WINDOW  lcForm + " - Btn Elimina - " + lcTime NOWAIT
							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								PUBLIC lnResult
								lnResult	= _SCREEN.ACTIVEFORM.lbRes
								* WAIT WINDOW  lcForm + " - Btn Elimina - " + lcTime &&NOWAIT
								IF lnResult = .T.
									* MESSAGEBOX("CONFIRMACION DB" + CHR(13) + "Respuesta: " + TRANSFORM(lnResult), 64, "POLICIA 1")
								ELSE && lnResult = .T.
									* MESSAGEBOX("CONFIRMACION DB" + CHR(13) + "Respuesta: " + TRANSFORM(lnResult), 64, "POLICIA 2")
								ENDIF && lnResult = .T.
							*<-- lcTime == "DESPUES"
						OTHERWISE && ELIMINACI�N Y TIEMPOS
						ENDCASE && ELIMINACI�N Y TIEMPOS
					*<-- lnFButton = 2 && ELIMINA

					CASE lnFButton = 4	&&GRABAR
						DO CASE &&GRABAR Y TIEMPOS
							WAIT WINDOW  lcForm + " - Btn Grabar - " + lcTime
							CASE lcTime == "ANTES"
								
							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								lnResult	= _SCREEN.ACTIVEFORM.lbRes
								IF lnResult = .T.
								ELSE && lnResult = .T.
								ENDIF && lnResult = .T.
							*<-- lcTime == "DESPUES"
						OTHERWISE &&GRABAR Y TIEMPOS
						ENDCASE &&GRABAR Y TIEMPOS
					*<-- && lnFButton = 4	&&GRABAR
				ENDCASE && Que Boton Presiono			
			*<-- lcForm == "PEDIDOS_P"
		*<-- VALIDADORES PARA FORMULARIO DE PEDIDOS DE COMPRA

		*--> VALIDADORES PARA FORMULARIO DE ARTICULOS
			CASE lcForm == "ARTICULOS"
				lbEcommerce	= _SCREEN.ACTIVEFORM.Pageframe1.Page5.ECOMMERCE.VALUE
				lbMaki		= _SCREEN.ACTIVEFORM.Pageframe1.Page5.MAKI.VALUE
				lbServicio	= _SCREEN.ACTIVEFORM.Pageframe1.Page5.SERVICIO.VALUE
				
				*!* IF lbServicio = .F.
					IF lbEcommerce = .T. OR lbMaki = .T.
						DO CASE	&& EVENTOS
							CASE _SCREEN.ACTIVEFORM.button = 5 && NUEVO
								IF  lcTime == "ANTES"
								ELSE &&lcTime == "ANTES"
								ENDIF &&lcTime == "ANTES"
							&& _SCREEN.ACTIVEFORM.button = 5 && NUEVO
							
							CASE _SCREEN.ACTIVEFORM.button = 6 && Elimina
								* WAIT WINDOW  lcForm + " - Btn Elimina - " + lcTime NOWAIT
								DO CASE &&ELIMINACI�N Y TIEMPOS
									CASE lcTime == "ANTES"
									&& lcTime == "ANTES"

									CASE lcTime == "DESPUES"
										IF lnResult = .T.
										ELSE && lnResult = .T.
										ENDIF && lnResult = .T.
									&& lcTime == "DESPUES"
								OTHERWISE && ELIMINACI�N Y TIEMPOS
								ENDCASE && ELIMINACI�N Y TIEMPOS
							&& _SCREEN.ACTIVEFORM.button = 6 && Elimina

							CASE _SCREEN.ACTIVEFORM.button = 7 && Modifica
								DO CASE &&MODIFICA Y TIEMPOS
									CASE lcTime == "ANTES"
									*<-- lcTime == "ANTES"
								OTHERWISE && MODIFICA Y TIEMPOS
								ENDCASE && MODIFICA Y TIEMPOS
							&& _SCREEN.ACTIVEFORM.button = 7 && Modifica
							
							CASE lnFButton = 8 OR _SCREEN.ACTIVEFORM.button = 8 &&GRABAR
								PUBLIC lnResult
								lnResult	= _SCREEN.ACTIVEFORM.lbRes

								DO CASE &&GRABAR Y TIEMPOS
									CASE lcTime == "ANTES"
									*<-- lcTime == "ANTES"

									CASE lcTime == "DESPUES"
										lcCodArt = ALLTRIM(articulos.codart)
										lnComercioEc = 0
										lnComercioMa = 0
										lnComercio = 0

										*!* --> Validamos ecommerce y maki
										SELECT ecommerce, maki FROM articulos WHERE ALLTRIM(codart) == lcCodArt AND NOT DELETED() AND (ecommerce = .T. OR maki = .T.) INTO CURSOR curMaki

										SELECT * FROM articulo_comercio WHERE ALLTRIM(codart) == lcCodArt AND NOT DELETED() INTO CURSOR curComercio
										IF curMaki.ecommerce = .T.
											IF RECCOUNT("curComercio") > 0
												lnComercioEc = 1
											ELSE 
												cMessageTitle = 'ARTÍCULO - COMERCIO'
												cMessageText = 'No tiene seleccionado ningun comercio. Desea continuar?'
												nDialogType = 4 + 32 + 256
												* 4 = Yes and No buttons
												* 32 = Question mark icon
												* 256 = Second button is default

												nAnswer = MESSAGEBOX(cMessageText, nDialogType, cMessageTitle)

												DO CASE
													CASE nAnswer = 6
														lnComercioEc = 1
													CASE nAnswer = 7
														lnComercioEc = 0
												ENDCASE 
											ENDIF && RECCOUNT("curComercio") > 0
										ENDIF && curMaki.ecommerce = .T.

										IF curMaki.maki = .T. 
											lnComercioMa = 1
										ENDIF && curMaki.maki = .T. 
										*!* <-- Validamos ecommerce y maki

										*!* --> Validamos Procesos
										DO CASE 
											CASE lnComercioEc = 1 AND lnComercioMa = 1
												lnComercio = 1
											CASE lnComercioEc = 1 AND lnComercioMa = 0
												lnComercio = 1
											CASE lnComercioEc = 0 AND lnComercioMa = 1
												lnComercio = 1
											CASE lnComercioEc = 0 AND lnComercioMa = 0
												lnComercio = 0
										ENDCASE 
									*!* <-- Validamos Procesos

										DO CASE
											CASE lnComercio = 1 && --> CASE lnComercio = 1 
												*<-- Verificamos si debe insertar o actualizar
												SELECT sync, update, idexterno FROM articulos WHERE ALLTRIM(codart) == lcCodArt INTO CURSOR qSync
												SELECT qSync

												IF qSync.sync = .F. AND qSync.update = .F.
													IF qSync.idexterno > 0
														lnUBtn = 7
													ELSE
														lnUBtn = 5
													ENDIF 
												ENDIF

												IF qSync.sync = .F. AND qSync.update = .T.
													lnUBtn = 7
												ENDIF
												*<-- Verificamos si debe insertar o actualizar

												IF lnResult = .T.
													IF lnUBtn = 5	&& Nuevo
														* UPDATE articulos SET ecommerce = .T. WHERE codart = lcCodArt
														* TABLEUPDATE(.T., .T., "articulos")
													ENDIF	&& Fin Nuevo

													IF lnUBtn = 7	&& Modifica
														
														REPLACE sync WITH .F. FOR ALLTRIM(codart) == lcCodArt IN articulos
														REPLACE update WITH .T. FOR ALLTRIM(codart) == lcCodArt IN articulos
														*REPLACE ecommerce WITH .T. FOR codart = lcCodArt IN articulos
														
														TABLEUPDATE(.T., .T., "articulos")

													ENDIF	&& Fin Modifica

													SELECT grupo, grupo4 FROM articulos WHERE ALLTRIM(codart) == lcCodArt INTO CURSOR qValidaCat READWRITE 
													SELECT qValidaCat

													IF !EMPTY(qValidaCat.grupo) AND !EMPTY(qValidaCat.grupo4)

														*!*	DECLARACION DEL EJECUTABLE PARA LA INTEGRACION
														lsTable =	["]+'articulos'+["] 
														lsCode	=	["]+lcCodArt+["]
														lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
														lbResp = oShell.Run(lsCadena,1,.T.)

														IF lbResp = 1
															WAIT WINDOW "NO SE PUDO SINCRONIZAR" TIMEOUT 1
														ELSE
														*----------->carga de stock
															IF lbServicio = .F.
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
																	lsTable =	["]+'articulos_hijos'+["] 
																	lsCode	=	["]+ALLTRIM(qChild.codart)+["]
																	lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
																	lbResp = oShell.Run(lsCadena,1,.T.)
																	
																	IF lbResp = 0
																		*----------->carga de stock
																		agregarStock(qChild.codart)

																		lsTable =	["]+'kardex'+["] 
																		lsCode	=	["]+ALLTRIM(qChild.codart)+["]
																		lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
																		lbResp = oShell.Run(lsCadena,1,.T.)
																		
																		IF lbResp = 1
																			*!* MESSAGEBOX(oStockRr.sms, "RESULTADO")
																			WAIT WINDOW "NO SE SINCRONIZO NINGUN VALOR DE STOCK DEL PRODUCTO HIJO" TIMEOUT 1
																		ENDIF
																	*<-----------fin de carga de stock
																	ENDIF
																ENDSCAN
															ENDIF && RECCOUNT('qChild') > 0

															WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
														ENDIF
													ELSE
														MESSAGEBOX("[CATEGORIA] Y [CATEGORIA ADICIONAL] SON DATOS OBLIGATORIOS PARA LA SINCRONIZACION")
													ENDIF
												ENDIF && lnResult = .T.
											&& <-- CASE lnComercio = 1

											CASE lnComercio = 0 && --> CASE lnComercio = 0
												WAIT WINDOW "NO SE SINCRONIZO EL PRODUCTO... DEBE SELECCIONAR EL COMMERCIO" TIMEOUT 1  
											&& <-- CASE lnComercio = 0
										ENDCASE 
										
									*<-- lcTime == "DESPUES"
								OTHERWISE &&GRABAR Y TIEMPOS
								ENDCASE &&GRABAR Y TIEMPOS

								&& STORE SPACE(1) TO _SCREEN.ACTIVEFORM.lbRes, lnResult, _SCREEN.ACTIVEFORM.varvalida1, lcTable, lcDirTable, lcDirPRG, lcTdb 
							*<-- && _SCREEN.ACTIVEFORM.button = 8	&&GRABAR
						ENDCASE && Boton Presinado					
						*<-- lcForm == "CLIENTES1_C"
					ENDIF && lbEcommerce = .T. OR lbMaki = .T.
				*!* ENDIF && lbServicio = .T.

		*--> VALIDADORES PARA FORMULARIO DE ARTICULOS

		*--> VALIDADORES PARA FORMULARIO DE CATEGORIA
			CASE ALLTRIM(UPPER(lcForm)) == "CATEGORIA"
				DO CASE	&& Que Boton Presiono
					CASE lnFButton = 1	&&NUEVO
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 1	&&NUEVO

					CASE lnFButton = 2 && ELIMINA
						* 
					*<-- lnFButton = 2 && ELIMINA

					CASE lnFButton = 3 && MODIFICA
						IF lcTime == "ANTES"
							* 
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 3 && MODIFICA

					CASE lnFButton = 4 && GRABAR
						DO CASE && GRABAR Y TIEMPOS
							CASE lcTime == "ANTES"
								* 
							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								PUBLIC lnResult
								lnResult = _SCREEN.ACTIVEFORM.lbRes
								lcGrupo = ALLTRIM(categoria.grupo)

								IF lnResult = .T.
									IF lnUBtn = 1 && Nuevo
									ENDIF	&& Fin Nuevo

									IF lnUBtn = 3 && Modifica
										REPLACE SYNC WITH .F. FOR ALLTRIM(grupo) == lcGrupo IN categoria
										REPLACE UPDATE WITH .T. FOR ALLTRIM(grupo) == lcGrupo IN categoria
										TABLEUPDATE(.T., .T., "categoria")
									ENDIF	&& Fin Modifica

									lsTable =	["]+'categoria'+["] 
									lsCode	=	["]+ALLTRIM(lcGrupo)+["]
									lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
									lbResp = oShell.Run(lsCadena,1,.T.)
									
									IF lbResp = 0
										WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
									ENDIF
								ELSE && lnResult = .T.
								ENDIF && lnResult = .T.
							*<-- lcTime == "DESPUES"

						OTHERWISE &&  && GRABAR Y TIEMPOS
						ENDCASE && GRABAR Y TIEMPOS
					*<-- lnFButton = 4 && GRABAR

					CASE lnFButton = 5 && CANCELA
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 5 && CANCELA
				OTHERWISE && Que Boton Presiono
				ENDCASE	&& Que Boton Presiono
			*<-- ALLTRIM(lcOrigen) == "CATEGORIA"
		*<-- VALIDADORES PARA FORMULARIO DE CATEGORIA

		*--> VALIDADORES PARA FORMULARIO DE INGRESO_PRODUCCION
			CASE lcForm == "INGRESO_PRODUCCION2"
				DO CASE	
					CASE lnFButton = 1
						WAIT WINDOW  "Nuevo ENCPROD-PROD.DIRECTA"
					CASE lnFButton = 3
						WAIT WINDOW  "Modifica ENCPROD-PROD.DIRECTA"
					CASE lnFButton = 2
						WAIT WINDOW  "Elimina ENCPROD-PROD.DIRECTA"
				ENDCASE && Boton Presinado				
		*--> VALIDADORES PARA FORMULARIO DE INGRESO_PRODUCCION

		*--> VALIDADORES PARA FORMULARIO DE COBROS MULTIPLES
			CASE lcForm == "CTASXC_M"
				DO CASE && Boton Presinado
					CASE lnFButton = 1 && NUEVO
						lcEvent = "NUEVO"
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 1 && NUEVO

					CASE lnFButton = 3 && MODIFICA
						lcEvent = "MODIFICACION"
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 3 && MODIFICA

					CASE lnFButton = 2 && ELIMINA
						lcEvent = "ELIMINACION"
						
					*<-- lnFButton = 2 && ELIMINA

					CASE lnFButton = 4 && GRABAR
						lcEvent = "GRABAR"
						DO CASE && GRABAR Y TIEMPOS
							CASE lcTime == "ANTES"
								
							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								PUBLIC lnResult
								lnResult = _SCREEN.ACTIVEFORM.lbRes
								IF lnResult = .T.
									lcNumCob = ALLTRIM(enccobm.docmulti)
									
									IF lnUBtn = 1 && Nuevo
										lsTable =	["]+'enccobm'+["] 
										lsCode	=	["]+ALLTRIM(lcNumCob)+["]
										lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
										lbResp = oShell.Run(lsCadena,1,.T.)
										
										IF lbResp = 0
											MESSAGEBOX("PROCESO CONCLUIDO")
										ENDIF
									ENDIF	&& 	Fin Nuevo

								ELSE && lnResult = .T.
								ENDIF && lnResult = .T.
							*<-- lcTime == "DESPUES"
						OTHERWISE &&  && GRABAR Y TIEMPOS
						ENDCASE && GRABAR Y TIEMPOS
					*<-- lnFButton = 4 && GRABAR
				ENDCASE && Boton Presinado				
			*<-- lcForm == "CTASXC_M"
		*<-- VALIDADORES PARA FORMULARIO DE COBROS MULTIPLES

		*--> VALIDADORES PARA FORMULARIO DE PAGOS_M
			CASE lcForm == "PAGOS_M"
				DO CASE	
					CASE lnFButton = 1
						WAIT WINDOW  "Nuevo ENCPAGM"
					CASE lnFButton = 3
						WAIT WINDOW  "Modifica ENCPAGM"
					CASE lnFButton = 2
						WAIT WINDOW  "Elimina ENCPAGM"
				ENDCASE && Boton Presinado				
		*<-- VALIDADORES PARA FORMULARIO DE PAGOS_M

		*--> VALIDADORES PARA FORMULARIO DE PROFORMAS_C
			CASE lcForm == "PROFORMAS_C"
				DO CASE	
					CASE lnFButton = 1
						WAIT WINDOW  "Nuevo PROFORMAS_C"
					CASE lnFButton = 3
						WAIT WINDOW  "Modifica PROFORMAS_C"
					CASE lnFButton = 2
						WAIT WINDOW  "Elimina PROFORMAS_C"
				ENDCASE && Boton Presinado
		*<-- VALIDADORES PARA FORMULARIO DE PROFORMAS_C

		*--> VALIDADORES PARA INGRESOS Y SALIDAS
			CASE lcForm == "INGRESOS_C"
				PUBLIC lnResult, lcCodAlmAnt
				lnResult	= _SCREEN.ACTIVEFORM.lbRes
				lcNumfac	= ALLTRIM(encreq.numreq)
				lcTipinv 	= _SCREEN.ACTIVEFORM.COMBO2.VALUE

				TRY
					lcCodAlm2	= tlcDato2
				CATCH
					lcCodAlm2	= ''
				ENDTRY

				DO CASE	&& Que Boton Presiono 
					CASE lnFButton = 1	&&NUEVO
						IF  lcTime == "ANTES"
							* WAIT WINDOW  lcForm + " - Btn Nuevo - " + lcTime
						ELSE &&lcTime == "ANTES"
						ENDIF &&lcTime == "ANTES"

					CASE lnFButton = 3	&&MODIFICA
						IF lcTime == "ANTES"
							SELECT codalmacen FROM encreq WHERE ALLTRIM(numreq2) == ALLTRIM(_SCREEN.ACTIVEFORM.Numreq.VALUE) INTO CURSOR qCodAlnAnt 
							lcCodAlmAnt = ALLTRIM(qCodAlnAnt.codalmacen)
							* WAIT WINDOW  lcForm + " - Btn Modifica " + lcTime
						ELSE &&lcTime == "ANTES"
						ENDIF &&lcTime == "ANTES"

					CASE lnFButton = 2	&&ELIMINA
						*PUBLIC lnResult, lcMov, lcDirTable, lcTable, lcDirTable
						
						DO CASE &&ELIMINACI�N Y TIEMPOS
							CASE lcTime == "ANTES"
								SELECT idexterno FROM	tipoinv	WHERE ALLTRIM(tipinv) == "EI" INTO CURSOR qTipInvI READWRITE
								SELECT idexterno FROM	tipoinv	WHERE ALLTRIM(tipinv) == "SI" INTO CURSOR qTipInvS READWRITE

								IF lcTipinv = 'S'
									lnId = qTipInvI.idexterno
									lnTo = 1
								ELSE 
									IF lcTipinv = 'E'
										lnId = qTipInvS.idexterno
										lnTo = 0
									ENDIF
								ENDIF

								SELECT ;
									t.unico AS unico, ;
									t.codart as codart, ;
									t.codalmacen as codalmacen,	;
									ALLTRIM(lcNumfac), ;
									t.cantidad as existe_act, ;
									t.cantidad_d as existe_act2, ;
									lnId as tipinv, ;
									lnTo as tipoperacion, ;
									t.fecha AS fecha ;
								FROM ;
									traninv t ;
									INNER JOIN almacenes al ON (t.codalmacen = al.codalmacen) ;
									INNER JOIN articulos ar ON (t.codart = ar.codart) ;
								WHERE ;
									ALLTRIM(numdoc) == ALLTRIM(lcNumfac) ;
									AND (ar.ecommerce = .T. OR ar.maki = .T.) ;
									AND al.sync = .T. ;
								INTO CURSOR qTraninv READWRITE 
								
								SELECT qTraninv 
								SCAN 
									SELECT ;
										codart, ;
										ubicacion_alm ;
									FROM ;
										stock ;
									WHERE ;
										ubicacion_alm == ALLTRIM(qTraninv.unico) ;
										AND NOT DELETED() ;
									INTO CURSOR qStock READWRITE 

									IF RECCOUNT("qStock") = 0 
										INSERT INTO stock( ;
											codart, ;
											codalmacen, ;
											origen, ;
											existe_act, ;
											existe_act2, ;
											tipinv, ;
											tipoperacion, ;
											fecha) ;
										VALUES ( ;
											qTraninv.codart, ;
											qTraninv.codalmacen, ;
											qTraninv.unico, ;
											qTraninv.existe_act, ;
											qTraninv.existe_act2, ;
											qTraninv.tipinv, ;
											qTraninv.tipoperacion, ;
											qTraninv.fecha ;
										) 
									ENDIF 
								ENDSCAN 
							&& lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								IF lnResult = .T. 
									SELECT codalmacen FROM renreq WHERE ALLTRIM(numreq) == ALLTRIM(lcNumfac) GROUP BY codalmacen INTO CURSOR qCodAlmRr
									SELECT qCodAlmRr

									lsTable =	["]+'kardexAlmacen'+["] 
									lsCode	=	["]+ALLTRIM(qCodAlmRr.codalmacen)+["]
									lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
									lbResp = oShell.Run(lsCadena,1,.T.)
									
									IF lbResp = 1
										recalcularStock(ALLTRIM(qCodAlmRr.codalmacen), ALLTRIM(lcPahtDBCom))
									ELSE
										WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
									ENDIF
								ELSE &&lnResult = .T.
								ENDIF &&lnResult = .T.
							*<-- lcTime == "DESPUES"

						OTHERWISE && ELIMINACI�N Y TIEMPOS
						ENDCASE && ELIMINACI�N Y TIEMPOS
				
					CASE lnFButton = 4	&&GRABAR
						DO CASE	&& Antes y Despues
							CASE lcTime == "ANTES"
								SELECT ;
									t.unico AS unico, ;
									t.codart as codart, ;
									t.numdoc as numdoc, ;
									t.codalmacen as codalmacen, ;
									t.cantidad as existe_act ;
								FROM ;
									traninv t ;
									INNER JOIN almacenes al ON t.codalmacen = al.codalmacen ;
									INNER JOIN articulos ar ON ar.codart = t.codart ;
								WHERE ;
									ALLTRIM(numdoc) == ALLTRIM(lcNumfac) ;
									AND NOT DELETED() ;
									AND al.sync = .T. ;
								INTO CURSOR qTraninvAntI READWRITE 
								
								*!* Policia
								*!* BROWSE TITLE "ANTES"
							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								IF lnResult = .T.
										IF lnUBtn = 1 && Nuevo registro
											*!* INSERT INTO stock(codart, codalmacen, ubicacion_alm, existe_act, existe_act2, tipinv, tipoperacion, fecha) SELECT t.codart as codart, t.codalmacen as codalmacen,	al.nomalmacen as ubicacion_alm,	t.cantidad as existe_act, t.cantidad_d as existe_act2, (SELECT idexterno FROM	tipoinv	WHERE ALLTRIM(tipinv) == ALLTRIM(lcTipinv))  as tipinv, IIF(t.tipinv = 'E', 1, 0) as tipoperacion, t.fecha AS fecha FROM traninv t INNER JOIN almacenes al ON (t.codalmacen = al.codalmacen) INNER JOIN articulos ar ON (t.codart = ar.codart) WHERE ALLTRIM(numdoc) == ALLTRIM(lcNumfac) AND (ar.ecommerce = .T. OR ar.maki = .T.)
											SELECT ;
												t.unico AS unico, ;
												t.codart as codart, ;
												t.codalmacen as codalmacen,	;
												ALLTRIM(lcNumfac), ;
												t.cantidad as existe_act, ;
												t.cantidad_d as existe_act2, ;
												(SELECT idexterno FROM	tipoinv	WHERE ALLTRIM(tipinv) == ALLTRIM(lcTipinv))  as tipinv, ;
												IIF(lcTipinv = 'E', 1, 0) as tipoperacion, ;
												t.fecha AS fecha ;
											FROM ;
												traninv t ;
												INNER JOIN almacenes al ON (t.codalmacen = al.codalmacen) ;
												INNER JOIN articulos ar ON (t.codart = ar.codart) ;
											WHERE ;
												ALLTRIM(numdoc) == ALLTRIM(lcNumfac) ;
												AND (ar.ecommerce = .T. OR ar.maki = .T.) ;
												AND al.sync = .T. ;
											INTO CURSOR qTraninv READWRITE 
											
											*!* Policia
											*!* BROWSE

											SELECT qTraninv 
											SCAN 
												SELECT ;
													codart, ;
													ubicacion_alm ;
												FROM ;
													stock ;
												WHERE ;
													ubicacion_alm == ALLTRIM(qTraninv.unico) ;
													AND NOT DELETED() ;
												INTO CURSOR qStock READWRITE 

												IF RECCOUNT("qStock") = 0 
													INSERT INTO stock( ;
														codart, ;
														codalmacen, ;
														origen, ;
														existe_act, ;
														existe_act2, ;
														tipinv, ;
														tipoperacion, ;
														fecha) ;
													VALUES ( ;
														qTraninv.codart, ;
														qTraninv.codalmacen, ;
														qTraninv.unico, ;
														qTraninv.existe_act, ;
														qTraninv.existe_act2, ;
														qTraninv.tipinv, ;
														qTraninv.tipoperacion, ;
														qTraninv.fecha ;
													) 
												ENDIF 
											ENDSCAN 
											
											SELECT codalmacen FROM renreq WHERE ALLTRIM(numreq) == ALLTRIM(lcNumfac) GROUP BY codalmacen INTO CURSOR qCodAlmRr
											SELECT qCodAlmRr

											lsTable =	["]+'kardexAlmacen'+["] 
											lsCode	=	["]+ALLTRIM(qCodAlmRr.codalmacen)+["]
											lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
											lbResp = oShell.Run(lsCadena,1,.T.)
											
											IF lbResp = 1
												recalcularStock(ALLTRIM(qCodAlmRr.codalmacen), ALLTRIM(lcPahtDBCom))
											ELSE
												WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
											ENDIF

											IF ALLTRIM(lcTipinv) == "ST"
												SELECT idexterno FROM TIPOINV WHERE tipinv == 'ET' INTO CURSOR qTipoInvEn
												lnId = qTipoInvEn.idexterno
												
												SELECT qTraninv
												IF RECCOUNT("qStock") = 0 
													INSERT INTO stock( ;
														codart, ;
														codalmacen, ;
														origen, ;
														existe_act, ;
														existe_act2, ;
														tipinv, ;
														tipoperacion, ;
														fecha) ;
													SELECT ;
														qTraninv.codart, ;
														lcCodAlm2, ;
														qTraninv.unico, ;
														qTraninv.existe_act, ;
														qTraninv.existe_act2, ;
														lnId, ;
														1, ;
														qTraninv.fecha ;
													FROM ;
														qTraninv
												ENDIF 

												lsTable =	["]+'kardexAlmacen'+["] 
												lsCode	=	["]+ALLTRIM(lcCodAlm2)+["]
												lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
												lbResp = oShell.Run(lsCadena,1,.T.)
												
												IF lbResp = 1
													recalcularStock(ALLTRIM(lcCodAlm2), ALLTRIM(lcPahtDBCom))
												ELSE
													WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
												ENDIF
											ELSE && lcTipinv == "ST"
											ENDIF && lcTipinv == "ST"
											
										ENDIF && Fin Nuevo registro
										IF lnUBtn = 3 && Actualiza registro
											SELECT idexterno FROM	tipoinv	WHERE ALLTRIM(tipinv) == ALLTRIM(lcTipinv) INTO CURSOR qTipInvIA READWRITE
											SELECT idexterno FROM	tipoinv	WHERE ALLTRIM(tipinv) == "EI" INTO CURSOR qTipInvI READWRITE
											SELECT idexterno FROM	tipoinv	WHERE ALLTRIM(tipinv) == "SI" INTO CURSOR qTipInvS READWRITE
											
											SELECT qTraninvAntI

											*!*	CURSOR DESPUES DE REGISTRAR O ELIMINAR LINEAS
											SELECT ;
												t.unico AS unico, ;
												t.codart as codart, ;
												t.numdoc as numdoc, ;
												t.codalmacen as codalmacen, ;
												t.cantidad as existe_act ;
											FROM ;
												traninv t ;
												INNER JOIN almacenes al ON t.codalmacen = al.codalmacen ;
												INNER JOIN articulos ar ON ar.codart = t.codart ;												
											WHERE ;
												ALLTRIM(numdoc) == ALLTRIM(lcNumfac) ;
												AND NOT DELETED() ;
												AND al.sync = .T. ;
											INTO CURSOR qTraninvDesI READWRITE 

											CREATE CURSOR qResultante( ;
												unico varchar(10), ;
												codart varchar(25), ;
												numdoc varchar(10), ;
												codalmacen varchar(10), ;
												existe_act numeric(20,8), ;
												tipinv numeric(10,0), ;
												tipoperacion int(1) ;
											)
											
											IF lcTipinv = 'S'
												lnId = qTipInvI.idexterno
												lnTo = 1
											ELSE 
												IF lcTipinv = 'E'
													lnId = qTipInvS.idexterno
													lnTo = 0
												ENDIF
											ENDIF

											*!* OBTENER REGISTROS DIFERENTES
											INSERT INTO qResultante ;
											SELECT ;
												qta.unico AS unico, ;
												qta.codart as codart, ;
												qta.numdoc as numdoc, ;
												qta.codalmacen as codalmacen, ;
												qta.existe_act as existe_act, ;
												lnId as tipinv, ;
												lnTo as tipoperacion ;
											FROM ;
												qTraninvAntI qta ;
												LEFT JOIN qTraninvDesI qtd ON qta.unico = qtd.unico ;
											WHERE ;
												qtd.unico is null

											*!* OBTENER REGISTROS DIFERENTES
											INSERT INTO qResultante ;
											SELECT ;
												qtd.unico AS unico, ;
												qtd.codart as codart, ;
												qtd.numdoc as numdoc, ;
												qtd.codalmacen as codalmacen, ;
												qtd.existe_act as existe_act, ;
												(qTipInvIA.idexterno)  as tipinv, ;
												IIF(lcTipinv = 'E', 1, 0) as tipoperacion ;
											FROM ;
												qTraninvDesI qtd ;
												LEFT JOIN qTraninvAntI qta ON qtd.unico = qta.unico ;
											WHERE ;
												qta.unico is null

											*!* CURSOR CON LOS DATOS A REGISTRAR
											SELECT ;
												qres.codart, ;
												qres.codalmacen, ;
												qres.unico, ;
												qres.existe_act, ;
												qres.tipinv, ;
												qres.tipoperacion ;
											FROM ;
												qResultante qres ;
												INNER JOIN articulos ar ON qres.codart = ar.codart ;
											WHERE ;
												ar.ecommerce = .T. ;
												OR ar.maki = .T. ;
											INTO CURSOR qStock READWRITE 

											*!* REGISTRO A TABLA STOCK
											INSERT INTO stock( ;
												codart, ;
												codalmacen, ;
												origen, ;
												existe_act, ;
												tipinv, ;
												tipoperacion, ;
												sync) ;
											SELECT ;
												qStock.codart AS codart, ;
												qStock.codalmacen AS codalmacen, ;
												qStock.unico AS origen, ;
												qStock.existe_act AS existe_act, ;
												qStock.tipinv AS tipinv, ;
												qStock.tipoperacion AS tipoperacion, ;
												.F. AS sync ;
											FROM ;
												qStock

											SELECT codalmacen FROM renreq WHERE ALLTRIM(numreq) == ALLTRIM(lcNumfac) GROUP BY codalmacen INTO CURSOR qCodAlmRr
											SELECT qCodAlmRr
											*!* Policia

											lsTable =	["]+'kardexAlmacen'+["] 
											lsCode	=	["]+ALLTRIM(qCodAlmRr.codalmacen)+["]
											lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
											lbResp = oShell.Run(lsCadena,1,.T.)
											
											IF lbResp = 1
												recalcularStock(ALLTRIM(qCodAlmRr.codalmacen), ALLTRIM(lcPahtDBCom))
											ELSE
												WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
											ENDIF

											IF ALLTRIM(lcTipinv) == "ST"
												SELECT tipinv FROM qStock WHERE tipoperacion = 1 GROUP BY tipinv into cursor qTip1
												lcTip1 = qTip1.tipinv
												SELECT tipinv FROM qStock WHERE tipoperacion = 0 GROUP BY tipinv into cursor qTip2
												lcTip2 = qTip2.tipinv

												SELECT ;
													codart, ;
													IIF(tipoperacion = 0, lcCodAlm2, lcCodAlmAnt) as codalmacen, ;
													unico, ;
													existe_act, ;
													IIF(tipoperacion = 0, 1, 0) as tipoperacion , ;
													IIF(tipoperacion = 0,  lcTip1, lcTip2) as tipinv  ;
												FROM qStock ;
												INTO CURSOR qTransAut

												*!* REGISTRO A TABLA STOCK
												INSERT INTO stock( ;
													codart, ;
													codalmacen, ;
													origen, ;
													existe_act, ;
													tipinv, ;
													tipoperacion, ;
													sync) ;
												SELECT ;
													qTransAut.codart AS codart, ;
													qTransAut.codalmacen AS codalmacen, ;
													qTransAut.unico AS origen, ;
													qTransAut.existe_act AS existe_act, ;
													qTransAut.tipinv AS tipinv, ;
													qTransAut.tipoperacion AS tipoperacion, ;
													.F. AS sync ;
												FROM ;
													qTransAut 

												IF ALLTRIM(lcCodAlm2) != ALLTRIM(lcCodAlmAnt) 
													lsTable =	["]+'kardexAlmacen'+["] 
													lsCode	=	["]+ALLTRIM(lcCodAlm2)+["]
													lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
													lbResp = oShell.Run(lsCadena,1,.T.)
													
													IF lbResp = 1
														recalcularStock(ALLTRIM(lcCodAlm2), ALLTRIM(lcPahtDBCom))
													ELSE
														WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
													ENDIF
												ENDIF

												lsTable =	["]+'kardexAlmacen'+["] 
												lsCode	=	["]+ALLTRIM(lcCodAlmAnt)+["]
												lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
												lbResp = oShell.Run(lsCadena,1,.T.)
												
												IF lbResp = 1
													recalcularStock(ALLTRIM(lcCodAlmAnt), ALLTRIM(lcPahtDBCom))
												ELSE
													WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
												ENDIF
												
											ENDIF && ALLTRIM(lcTipinv) == "ST"
										ENDIF && Fin Actualiza registro
		
									ELSE &&lnResult = .T.
									ENDIF &&lnResult = .T.
							*<-- lcTime == "DESPUES"

							CASE lcTime == "FINAL"
								*MESSAGEBOX("Paso por Final")
							*<-- lcTime == "FINAL"

							OTHERWISE && Otro caso
						ENDCASE && Fin Do Case
						
					CASE lnFButton = 5	&&CANCELA
						IF lcTime == "ANTES"
							* WAIT WINDOW  lcForm + " - Cancela - " + lcTime
						ELSE &&lcTime == "ANTES"  	
						ENDIF &&lcTime == "ANTES"
					*<-- lnFButton = 5	&&CANCELA
				ENDCASE && Que Boton Presiono
				OTHERWISE
			*<-- CASE FORMULARIO INGRESOS_C
		*<-- VALIDADORES PARA INGRESOS Y SALIDAS

		*--> VALIDADORES PARA FORMULARIO DE TRANINV_ALM
			CASE ALLTRIM(UPPER(lcForm)) == "STOCK"
				DO CASE	&& Que Boton Presiono
					CASE lnFButton = 1	&&NUEVO
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 1	&&NUEVO

					CASE lnFButton = 2 && ELIMINA
						* 
					*<-- lnFButton = 2 && ELIMINA

					CASE lnFButton = 3 && MODIFICA
						IF lcTime == "ANTES"
							* 
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 3 && MODIFICA

					CASE lnFButton = 4 && GRABAR
						DO CASE && GRABAR Y TIEMPOS
							CASE lcTime == "ANTES"
								* 
							*<-- lcTime == "ANTES"

							CASE lcTime == "DESPUES"
								PUBLIC lnResult
								lnResult = _SCREEN.ACTIVEFORM.lbRes
								lcCodart = ALLTRIM(traninv_alm.codart)

								IF lnResult = .T.
									IF lnUBtn = 1 && Nuevo
									ENDIF	&& Fin Nuevo

									IF lnUBtn = 3 && Modifica
										REPLACE SYNC WITH .F. FOR codart = lcCodart IN traninv_alm
										TABLEUPDATE(.T., .T., "traninv_alm")
									ENDIF	&& Fin Modifica

									lsTable =	["]+'kardexAlmacen'+["] 
									lsCode	=	["]+ALLTRIM(lcGrupo)+["]
									lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
									lbResp = oShell.Run(lsCadena,1,.T.)
									
									IF lbResp = 1
										recalcularStock(ALLTRIM(lcGrupo), ALLTRIM(lcPahtDBCom))
									ELSE
										WAIT WINDOW "PROCESO CONCLUIDO" TIMEOUT 1
									ENDIF
								ELSE && lnResult = .T.
								ENDIF && lnResult = .T.
							*<-- lcTime == "DESPUES"

						OTHERWISE &&  && GRABAR Y TIEMPOS
						ENDCASE && GRABAR Y TIEMPOS
					*<-- lnFButton = 4 && GRABAR

					CASE lnFButton = 5 && CANCELA
						IF lcTime == "ANTES"
							
						ELSE && lcTime == "ANTES"
						ENDIF && lcTime == "ANTES"
					*<-- lnFButton = 5 && CANCELA
				OTHERWISE && Que Boton Presiono
				ENDCASE	&& Que Boton Presiono
			*<-- ALLTRIM(lcOrigen) == "CATEGORIA"
		*<-- VALIDADORES PARA FORMULARIO DE TRANINV_ALM

	OTHERWISE
		* WAIT WINDOW "No se encuentra definido este formulario"
	ENDCASE	&& _SCREEN.ACTIVEFORM = "Nombre de Formulario "
CATCH TO loExcep
	MESSAGEBOX(loExcep.MESSAGE)
ENDTRY



* TRY && FORMULARIOS MODALES - BASICOS
* 	DO CASE && FORMULARIOS MODALES
* 		WAIT WINDOW "Formulario: " + lcOrigen
		
* 	OTHERWISE && FORMULARIOS MODALES
* 	ENDCASE && FORMULARIOS MODALES
* CATCH && FORMULARIOS MODALES - BASICOS
* ENDTRY && FORMULARIOS MODALES - BASICOS

RETURN .T.

*------------
*!* FUNCIONES
*------------
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

*!* 
FUNCTION recalcularStock(lcCodAlmacen, lcPath)
	SELECT s.codart as codart FROM stock s INNER JOIN ARTICULOS a ON s.codart = a.codart WHERE s.sync = .F. AND a.idexterno > 0 INTO CURSOR curRecalculo

	SELECT curRecalculo
	SCAN
		oShell = CREATEOBJECT("WScript.Shell")
		lsRuta 	= 	["]+'C:\Fenix\AdsFenix\DATOS_pro\Datos_add\BR_eCommerce\API\InitIntegration.exe'+["]
		lsBD	=	["]+ALLTRIM(lcPath)+["]
		lsTable =	["]+'kardex'+["] 
		lsCode	=	["]+ALLTRIM(curRecalculo.codart)+["]
		lsCadena = lsRuta + " " + lsTable + " " + lsCode + " " + lsBD
		lbResp = oShell.Run(lsCadena,1,.T.)
		
		IF lbResp = 1
			WAIT WINDOW "ESPERE POR FAVOR - SINCRONIZANDO" TIMEOUT 1
		ENDIF 
	ENDSCAN  

	WAIT WINDOW "SINCRONIZACIÓN COMPLETA" TIMEOUT 1

	RETURN .T.
ENDFUNC