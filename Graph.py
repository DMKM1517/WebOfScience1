
# coding: utf-8

# In[1]:

import mysql.connector
import networkx as nx


# In[2]:

def find_neighbour(author):
    cnx = mysql.connector.connect(user='root', password='dmkm1234',
                                  host='localhost',
                                  database='dmkm_articles', port='8889')
    cursor = cnx.cursor()
    query = ("select s1.author_id as a_1, s2.author_id as a_2 "
                "from signature s1, signature s2 "
                "where s1.author_id != s2.author_id "
                "and s1.article_id = s2.article_id "
                "and s1.author_id ="+str(author)
                     )
    cursor.execute(query)
    exit = []
    for (a_1, a_2) in cursor:
        exit.append([a_1,a_2])
    cursor.close()
    cnx.close()
    return exit
def list_author(exit,author):
    flatten = list(set([val for sublist in exit for val in sublist]))
    try:
        flatten.remove(author)
    except ValueError:
        print(a)
    return flatten
def add_vicinity(graph,author):
    edges = find_neighbour(author)
    G.add_edges_from(edges)
    vicinity = list_author(edges,author)
    return vicinity
def graph_r(G,author,k):
    if k >= 1:
        vici = add_vicinity(G,author)
        for authors in vici:
            k = k - 1
            graph_r(G,authors,k)


# In[53]:

cnx = mysql.connector.connect(user='root', password='dmkm1234',
                                  host='localhost',
                                  database='dmkm_articles', port='8889')
cursor = cnx.cursor(buffered=True)
cursorb = cnx.cursor(buffered=True)

query = ("select a_1, a_2 from graph where distance is null limit 10")
cursor.execute(query)

query2 = ("update graph "
          "set distance = %s "
          "where a_1= %s and a_2= %s and distance is null "
                )
i=0
for (a_1,a_2) in cursor:
    G=nx.Graph()
    k=5
    graph_r(G,a_1,k)
    graph_r(G,a_2,k)
    i = i+1
    d = None
    try:
        d = str(len(nx.shortest_path(G,a_1,a_2))-1)
        cursorb.execute(query2,(d,a_1,a_2))
        cnx.commit()
        print(str(a_1)+' '+str(a_2)+' '+str(d)+' conn')
    except:
        pass
cursorb.close()
cursor.close()
cnx.close()


# In[344]:

#d=0
#query2 = ("update graph "
#                  "set distance = "+str(d)+" "
#                  "where a_1="+str(a_1)+" and a_2="+str(a_2)+" and distance is null ")
#print(query2)


# In[50]:

#a1=2
#a2=62182
#k=5
#G=nx.Graph()
#graph_r(G,a1,k)
#graph_r(G,a2,k)


# In[51]:

#len(G.nodes())


# In[52]:

#len(nx.shortest_path(G,a1,a2))-1


# In[353]:

#for author in cursor:
#    pass
#cursor1
#cnx.commit()
#cursor.close()
#cnx.close()


# In[235]:

#G.edges()


# In[236]:

#nx.clustering(G)


# In[ ]:




# In[ ]:



