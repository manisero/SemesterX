package pl.edu.pw.elka.spdb.repositories;

import org.springframework.data.neo4j.repository.GraphRepository;
import pl.edu.pw.elka.spdb.model.MapEntry;

public interface IMapEntryRepository extends GraphRepository<MapEntry> {
}
