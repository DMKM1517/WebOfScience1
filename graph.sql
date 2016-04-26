SELECT author, count(id)
from articles_authors
group by author
order by count(id) desc
;

Select articles.id as id_article, authors.rowid as id_author 
from articles, authors
;
/*
drop table authors;
SET @cnt = 0;
CREATE TABLE authors AS (
SELECT (@cnt := @cnt + 1) AS rowid, a.author from (
select distinct au.author
FROM articles_authors au) a
);
*/
select count(*)
from articles
;


select count(*)
from (
select distinct author
from dmkm_articles.articles_authors_disambiguated
) a
;

select count(*)
from (
select distinct author
from articles_authors
) a
;

select count(*)
from articles
;

drop table author_count;
create table author_count as ( 
SELECT author, count(id) 
from articles_authors
group by author
order by count(id) desc
)
;

select *
from author_count
limit 100
;

drop table authors;
SET @cnt = 0;
CREATE TABLE authors AS (
SELECT (@cnt := @cnt + 1) AS author_id, a.author from (
	select distinct au.author
	FROM articles_authors au) a
);

CREATE INDEX author_index ON articles_authors (author) USING HASH;
CREATE INDEX authors_index ON authors (author) USING HASH;
CREATE INDEX author_id_index ON authors (author_id) USING HASH;

select *
from authors
limit 100
;

select count(*) --318,591
from articles
;
/*--475,327*/
select count(*) 
from authors
;
/*--Articles per string*/
select avg(b.c) 
from (
	SELECT author, count(id) as c
	from articles_authors
	group by author
) as b
;

create table author_rank as (
select au.author_id, a.author, a.c
from (SELECT author, count(id) as c
from articles_authors
group by author	) a,
authors au
where a.author = au.author
order by a.c desc
)
;


drop table authors_per_article;
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

select *
from authors_per_article
limit 100
;

drop table journals;
SET @cnt = 0;
CREATE TABLE journals AS (
	SELECT (@cnt := @cnt + 1) AS journal_id, a.journal from (
		select distinct au.journal
		FROM articles au) a
);

/*--14,964 */
select count(*) 
from journals
;


CREATE INDEX author_index ON articles_authors (author) USING HASH;
CREATE INDEX articles_index ON articles (id) USING HASH;
CREATE INDEX journal_index on articles (journal) using HASH;
CREATE INDEX journal_index2 on journals (journal) using hash;

drop table signature;
SET @cnt = 0;
create table signature as (
	Select  (@cnt := @cnt + 1) AS signature_id, ar.id as article_id, aus.author_id
	from 	articles ar, articles_authors au, authors aus
	where 	ar.id = au.id 
	AND 	au.author = aus.author
	AND 	type = 'Article'
)
;

/*create indexes*/
CREATE INDEX signature ON signature (signature_id) USING HASH;
CREATE INDEX signature_article ON signature (article_id) USING HASH;
CREATE INDEX signature_author ON signature (author_id) using HASH;




/*1,724,465*/
/*2,973,779,536,225*/
select *
from signature
;

drop table signaturetrim;
create table signaturetrim as (select * from signature limit 100000);

drop table graph ;
create table graph as ( 
	select s1.signature_id as ss1, s2.signature_id as ss2
	from signature s1,	signature s2
	where s1.author_id != s2.author_id
	and s1.article_id = s2.article_id
	limit 1
)
;

alter table graph
add column distance int
;

truncate table graph;

select * 
from graph;

select count(*) from signature;




update graph  set distance = null where distance is not null;

select * from graph where distance is  not null;

update graph set distance = null where a_1='2' and a_2='155241' and distance is null ;
update graph set distance = '1' where a_1='2' and a_2='41704' and distance is null ;


select s1.author_id, s2.author_id
	from signature s1,	signature s2
	where s1.signature_id != s2.signature_id
	and s1.article_id = s2.article_id
	and s1.author_id = '2'
;

select count(*) from graph;

drop table nodes;
create table nodes as (
select distinct a_1 as id, a_1 as label, null as "interval"
from graph
)
;

select count(*) from nodes;

drop table edges;
create table edges as (
select a_1 as source , a_2 as target, 'Undirected' as type, null as id, null as label, 1 as weight
from graph
)
;

drop table author_lattice;
create table author_lattice as (
select  a1.author_id as a_1, a2.author_id as a_2
from authors a1,authors a2
	where a1.author_id != a2.author_id
	and a1.author_id < a2.author_id
	limit 1000000
)
;

alter table author_lattice
add column distance int
;

CREATE INDEX author_id_1_index ON author_lattice (a_1) USING HASH;
CREATE INDEX authors_id_2_index ON author_lattice (a_2) USING HASH;

select a_1, a_2
from signature
where a_1='6'
;

select *
from author_rank
where author_id = '11'
;

select *
from signature
order by article_id
;

select a_1, a_2 from (
select s1.author_id as a_1, s2.author_id as a_2
	from signature s1,	signature s2
	where s1.author_id != s2.author_id
	and s1.article_id = s2.article_id
	and s1.author_id = ''
	)a
	;
	
select *
from signature
where 	author_id = '243'

;

select * 
from signature
where article_id = '1'
;

select * 
from articles
where id = '117320'
;

select a_1,a_2
from author_lattice au, author_rank r
where au.a_1 = r.author_id
and distance is null
order by c desc
;

select * from author_rank;

select s1.author_id as a_1, s2.author_id as a_2
from signature s1, signature s2, author_rank r
where s1.author_id != s2.author_id
and s1.article_id = s2.article_id
and s1.author_id = r.author_id
limit 100000 offset (3*100000)
;

select count(*) from (
select s1.author_id as a_1, s2.author_id as a_2
from signature s1, signature s2
where s1.author_id != s2.author_id
and s1.article_id = s2.article_id
and s1.author_id = '25347'
)a
;

select *
from authors
;