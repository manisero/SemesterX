package pl.edu.pw.elka.spdb.dao;

import pl.edu.pw.elka.spdb.dao.location.ILocationDAO;

public interface IDAOFactory {
    ILocationDAO getLocationDAO();
}
