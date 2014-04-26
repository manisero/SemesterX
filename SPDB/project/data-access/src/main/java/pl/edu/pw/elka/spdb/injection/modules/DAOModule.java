package pl.edu.pw.elka.spdb.injection.modules;

import com.google.inject.AbstractModule;
import pl.edu.pw.elka.spdb.dao.IDAOFactory;
import pl.edu.pw.elka.spdb.dao.impl.Neo4jDAOFactory;

public class DAOModule extends AbstractModule {
    @Override
    protected void configure() {
        bind(IDAOFactory.class).to(Neo4jDAOFactory.class);
    }
}
