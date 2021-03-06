TRUNCATE TABLE [dbo].[Diagnosis];
TRUNCATE TABLE [dbo].[Patient];
TRUNCATE TABLE [dbo].[Admission];
TRUNCATE TABLE [dbo].[Discharge];
TRUNCATE TABLE [dbo].[Provider];




ALTER TABLE [Group12_Heart].[dbo].[Diagnosis] ADD [NEW_PRINC_DIAG_CODE]
 AS (
  CASE
   WHEN [PRINC_DIAG_CODE] BETWEEN 390 AND 392 THEN 'RHEUMATIC FEVER'
   WHEN [PRINC_DIAG_CODE] BETWEEN 393 AND 398 THEN 'Chronic rheumatic heart disease'
   WHEN [PRINC_DIAG_CODE] BETWEEN 401 AND 405 THEN 'Hypertensive disease'
   WHEN [PRINC_DIAG_CODE] BETWEEN 410 AND 414 THEN 'Ischemic heart disease'
   WHEN [PRINC_DIAG_CODE] BETWEEN 415 AND 417 THEN 'Diseases of pulmonary circulation'
   WHEN [PRINC_DIAG_CODE] BETWEEN 420 AND 429 THEN 'Other forms of heart disease'
   WHEN [PRINC_DIAG_CODE] BETWEEN 430 AND 438 THEN 'Cerebrovascular disease'
   WHEN [PRINC_DIAG_CODE] BETWEEN 440 AND 448 THEN 'Diseases of arteries, arterioles, and capillaries'
   WHEN [PRINC_DIAG_CODE] BETWEEN 451 AND 459 THEN 'Diseases of veins and lymphatics, and other diseases of circulatory system'
   END 
   )

	


   

    



