package pl.edu.pw.elka.spdb.tests.dao.entries;

import junit.framework.TestCase;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.neo4j.support.Neo4jTemplate;
import org.springframework.data.neo4j.support.node.Neo4jHelper;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.transaction.BeforeTransaction;
import org.springframework.transaction.annotation.Transactional;
import pl.edu.pw.elka.spdb.dao.entries.IMapEntryDAO;
import pl.edu.pw.elka.spdb.model.MapEntry;

@ContextConfiguration(locations = "classpath:/spring/testContext.xml")
@RunWith(SpringJUnit4ClassRunner.class)
@Transactional
public class Neo4jMapEntryDAOTests extends TestCase {
    @Autowired private IMapEntryDAO mapEntryDAO;
    @Autowired private Neo4jTemplate template;

    @Rollback(false)
    @BeforeTransaction
    public void cleanUp() {
        Neo4jHelper.cleanDb(template);
    }

    @Test
    public void testInsertAndFindLocationMethods() {
        MapEntry mapEntry = new MapEntry(52.2206062, 21.0105747);

        Long mapEntryId = mapEntryDAO.insertMapEntry(mapEntry).getId();
        MapEntry foundMapEntry = mapEntryDAO.findMapEntryById(mapEntryId);

        assertNotNull(foundMapEntry);
        assertEquals(mapEntry, foundMapEntry);
    }
}
