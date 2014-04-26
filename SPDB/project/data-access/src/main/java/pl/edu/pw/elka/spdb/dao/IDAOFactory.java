package pl.edu.pw.elka.spdb.dao;

import pl.edu.pw.elka.spdb.dao.entries.IMapEntryDAO;

public interface IDAOFactory {
    IMapEntryDAO getMapEntryDAO();
}
