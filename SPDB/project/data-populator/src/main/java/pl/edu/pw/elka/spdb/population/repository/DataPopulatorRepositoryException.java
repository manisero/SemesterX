package pl.edu.pw.elka.spdb.population.repository;

public class DataPopulatorRepositoryException extends RuntimeException {
    public DataPopulatorRepositoryException(String message) {
        super(message);
    }

    public DataPopulatorRepositoryException(String message, Throwable cause) {
        super(message, cause);
    }
}
