package pl.edu.pw.elka.spdb.tests.configuration;

import junit.framework.TestCase;
import org.junit.Test;
import pl.edu.pw.elka.spdb.configuration.IConfigurationProvider;
import pl.edu.pw.elka.spdb.configuration.impl.ConfigurationProvider;

import java.util.Properties;

public class ConfigurationProviderTests extends TestCase {
    @Test
    public void testGetDatabasePathMethod() {
        Properties properties = new Properties();
        properties.setProperty("database_path", "dbpath");
        IConfigurationProvider configurationProvider = new ConfigurationProvider(properties);

        String databasePath = configurationProvider.getDatabasePath();

        assertEquals("dbpath", databasePath);
    }
}
