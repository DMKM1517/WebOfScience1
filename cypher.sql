LOAD CSV WITH HEADERS FROM "http://poincare:8888/signature/signaturetrimcast.csv" AS csvLine
CREATE (s:Signature { 
	signature_id: toInt(csvLine.signature_id), 
	article_id: toInt(csvLine.article_id),
	author_id: toInt(csvLine.author_id)
	});

CREATE INDEX ON :Signature(signature_id)
CREATE INDEX ON :Signature(article_id)
CREATE INDEX ON :Signature(author_id)


MATCH (s1:Signature),(s2:Signature)
WHERE s1.article_id = s2.article_id
AND	  s1.author_id <> s2.author_id 
CREATE (s1)-[r:COAUTHOR {article_id : s1.author_id + '<->' + s2.author_id}] -> (s2)
RETURN r

MATCH (s1:Signature)-[r:COAUTHOR]-(s2:Signature)
RETURN (r)

MATCH (s1)
DETACH DELETE s1