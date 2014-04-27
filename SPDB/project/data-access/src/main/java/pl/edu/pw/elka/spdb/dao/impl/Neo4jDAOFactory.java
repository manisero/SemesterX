package pl.edu.pw.elka.spdb.dao.impl;

import pl.edu.pw.elka.spdb.dao.IDAOFactory;
import pl.edu.pw.elka.spdb.dao.entries.IMapEntryDAO;
import pl.edu.pw.elka.spdb.dao.entries.impl.Neo4jMapEntryDAO;

public class Neo4jDAOFactory implements IDAOFactory {
    @Override
    public IMapEntryDAO getMapEntryDAO() {
        return new Neo4jMapEntryDAO();
    }
}
