Course comparison app
===

Setting up
---

Make sure that the Java is correctly configured with R. You can execute this script as root:

```
R CMD javareconf
```

Required packages:

- ``ggplot2``
- ``shiny``
- ``RJDBC``

This may be useful to load Java, at least on OS X:

```
dyn.load("/Library/Java/JavaVirtualMachines/jdk1.8.0_51.jdk/Contents/Home/jre/lib/server/libjvm.dylib")
```

See http://stackoverflow.com/a/34488965/3576976 for reference.

Set up ``config.R`` basing on ``config.R.example``.

You will also need Oracle JDBC jar.

To forward database from students server to your desktop execute command:

```
ssh -N -L localhost:db-server:1521 students
```

Where ``db-server`` is hostname of database server, which is only accessible via students.