# asos_reader
read asos1 file

At the end i want to use it for CALPUFF.

Taking a lot from AERMINUTE
https://www.epa.gov/scram/meteorological-processors-and-accessory-programs

Unclear what licens it has, this is what  i see in the code

    c      program AERMINUTE
    c**********************************************************************************
    c     Purpose:  To read 2-minute average ASOS winds and calculate hourly averages for
    c               substitution into AERMET
    c               AERMOD input
    c     Programmer:  James Thurman US EPA, OAR/OAQPS/AQAD/AGMG
    c
    c     Version:  15272
    c     Description:
    c     This program reads the 2-minute average ASOS winds, recorded every minute
    c     and uses the even minutes to calculate an hourly average wind to be substituted
    c     into AERMET stage 1 and stage 3 output files
