FOR /l %%I IN (1,1,100) DO "C:\Program Files\R\R-3.1.2\bin\x64\Rscript.exe" AHA_Proxy_batch.R 50*(%%I-1)+1 50*%%I





