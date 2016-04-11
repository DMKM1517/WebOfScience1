CREATE TABLE articles_1 AS (
SELECT  au.author, ar.title, ar.id
FROM articles ar, articles_authors au
WHERE ar.id = au.id )
;

create table author_count as ( 
SELECT author, count(id)
from articles_authors
group by author
order by count(id) desc
)
;

drop table authors;
SET @cnt = 0;
CREATE TABLE authors AS (
SELECT (@cnt := @cnt + 1) AS rowid, a.author from (
select distinct au.author
FROM articles_authors au) a
);

select count(*)
from articles
;

select count(*) 
from authors
;

select avg(b.c) 
from (
SELECT author, count(id) as c
from articles_authors
group by author
) as b
;

create table authors_per_article as ( 
select b.c, count(b.c) 
from (
SELECT author, count(id) as c
from articles_authors
group by author
) as b
group by b.c
)
;

drop table journals;
SET @cnt = 0;
CREATE TABLE journals AS (
SELECT (@cnt := @cnt + 1) AS rowid, a.journal from (
select distinct au.journal
FROM articles au) a
);

select count(*) 
from journals
;


CREATE INDEX author_index ON articles_authors (author) USING HASH;
CREATE INDEX articles_index ON articles (id) USING HASH;
CREATE INDEX journal_index on articles (journal) using HASH;


Select ar.id as article_id, aus.rowid as author_id
from articles ar, articles_authors au, authors aus
where ar.id = au.id 
AND au.author = aus.author
AND type = 'Article'
;


