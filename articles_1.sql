SET @cnt = 0;
CREATE TABLE articles_1 AS (
SELECT  (@cnt := @cnt + 1) AS rowid, au.author, ar.title, ar.id
FROM articles ar, articles_authors au
WHERE ar.id = au.id 
)
