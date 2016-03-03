CREATE TABLE articles_1 AS (
SELECT  au.author, ar.title, ar.id
FROM articles ar, articles_authors au
WHERE ar.id = au.id )
