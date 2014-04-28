package pl.edu.pw.elka.spdb.population.impl;

import org.neo4j.graphdb.Transaction;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.neo4j.support.Neo4jTemplate;
import org.springframework.data.neo4j.support.node.Neo4jHelper;
import pl.edu.pw.elka.spdb.dao.entries.IMapEntryDAO;
import pl.edu.pw.elka.spdb.population.DataPopulatorException;
import pl.edu.pw.elka.spdb.population.IDataPopulator;
import pl.edu.pw.elka.spdb.population.repository.IDataPopulatorRepository;

public class DataPopulator implements IDataPopulator {
    @Autowired
    private IDataPopulatorRepository repository;

    @Autowired
    private IMapEntryDAO mapEntryDAO;

    @Autowired
    private Neo4jTemplate template;

    @Override
    public void populate() {
        try {
            emptyDatabase();
            populateEntries();
            populateRoutes();
        } catch (Exception e) {
            throw new DataPopulatorException("Could not populate database", e);
        }
    }

    private void emptyDatabase() {
        Neo4jHelper.cleanDb(template);
    }

    private void populateEntries() {
        repository.getEntries().forEach(mapEntryDAO::insertMapEntry);
    }

    private void populateRoutes() {
        Transaction transaction = template.getGraphDatabaseService().beginTx();
        repository.getRoutes().forEach(template::save);
        transaction.success();
    }
}
