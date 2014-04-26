package pl.edu.pw.elka.spdb.dao.impl;

import pl.edu.pw.elka.spdb.dao.IDAOFactory;
import pl.edu.pw.elka.spdb.dao.location.ILocationDAO;
import pl.edu.pw.elka.spdb.dao.location.impl.Neo4jLocationDAO;

public class Neo4jDAOFactory implements IDAOFactory {
    @Override
    public ILocationDAO getLocationDAO() {
        return new Neo4jLocationDAO();
    }
}
