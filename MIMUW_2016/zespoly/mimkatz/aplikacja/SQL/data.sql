set linesize 1000;
spool /home/staff/mim/mk305152/jnp2/bd/ankiety_raw.txt;
select KOD, PRZ_NAZWA, ZAJ_KOD, PROWADZACY, WARTOSC_ODPOWIEDZI, LICZBA_ODPOWIEDZI, TRESC_PYTANIA, CDYD_KOD from rstat_czestosci_ankiet where KOD like '%1000-%';
spool off;
spool /home/staff/mim/mk305152/jnp2/bd/oceny_raw.txt;
select KOD, PRZ_NAZWA, CDYD_KOD, NUMER_TERMINU, OCENA, OSOBA from rstat_oceny where jed_org_kod_biorca=10000000;
spool off;
