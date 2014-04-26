package pl.edu.pw.elka.spdb.configuration.impl;

import pl.edu.pw.elka.spdb.configuration.ConfigurationProviderException;
import pl.edu.pw.elka.spdb.configuration.IConfigurationProvider;

import java.io.IOException;
import java.util.Properties;

public class ConfigurationProvider implements IConfigurationProvider {
    private static final String CONFIGURATION_RESOURCE_NAME = "config.properties";
    private final Properties properties;

    public ConfigurationProvider() {
        try {
            properties = new Properties();
            properties.load(getClass().getClassLoader().getResourceAsStream(CONFIGURATION_RESOURCE_NAME));
        } catch (IOException e) {
            throw new ConfigurationProviderException("Could not open config.properties file!", e);
        }
    }

    public ConfigurationProvider(Properties properties) {
        this.properties = properties;
    }

    @Override
    public String getDatabasePath() {
        return properties.getProperty("database_path");
    }
}
