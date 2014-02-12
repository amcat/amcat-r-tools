source("~/amcat-r-tools/codebook_tools.r")

conn = amcat.connect("http://localhost:8000", "amcat", "amcat")
conn = amcat.connect("http://amcat.vu.nl", Sys.getenv('USER'))

x = amcat.gethierarchy(conn=conn, codebook_id=1, languages=c("en", "query"))


x = codebook.add.cats(x, maxdepth=1)
q = codebook.getqueries(x$label.query, x$cat.1)

h = amcat.hits(conn, q$query, q$label, sets=2) 

c = amcat.aggregate(conn, q$query, labels=q$label, sets=6263, axis1="year")
c = merge(x, c, by.x="code", by.y="query", all.x=T)
