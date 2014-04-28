package pl.edu.pw.elka.spdb.dao.entries.impl;

import org.neo4j.graphalgo.GraphAlgoFactory;
import org.neo4j.graphalgo.PathFinder;
import org.neo4j.graphalgo.WeightedPath;
import org.neo4j.graphdb.Direction;
import org.neo4j.graphdb.DynamicRelationshipType;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.PathExpanders;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.neo4j.conversion.EndResult;
import org.springframework.data.neo4j.support.Neo4jTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.edu.pw.elka.spdb.configuration.IConfigurationProvider;
import pl.edu.pw.elka.spdb.dao.entries.IMapEntryDAO;
import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.relationships.MapEntryRelationships;
import pl.edu.pw.elka.spdb.repositories.IMapEntryRepository;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

@Service
@Transactional
public class Neo4jMapEntryDAO implements IMapEntryDAO {
    @Autowired
    private IMapEntryRepository mapEntryRepository;
    @Autowired
    private Neo4jTemplate template;
    @Autowired
    private IConfigurationProvider configurationProvider;

    @Override
    public MapEntry insertMapEntry(MapEntry mapEntry) {
        return mapEntryRepository.save(mapEntry);
    }

    @Override
    public MapEntry findMapEntryById(Long id) {
        return mapEntryRepository.findOne(id);
    }

    @Override
    public List<MapEntry> findFastestRoute(MapEntry start, MapEntry end) {
        List<MapEntry> fastestRoute = new ArrayList<>();

        WeightedPath shortestPath = getShortestPath(template.getNode(start.getId()), template.getNode(end.getId()));
        shortestPath.nodes().forEach(node -> fastestRoute.add(findMapEntryById(node.getId())));

        return fastestRoute;
    }

    private WeightedPath getShortestPath(Node start, Node end) {
        PathFinder<WeightedPath> finder = GraphAlgoFactory.dijkstra(PathExpanders.forTypeAndDirection
                (DynamicRelationshipType.withName(MapEntryRelationships.ROUTES_TO.getValue()),
                        Direction.OUTGOING), "duration");

        return finder.findSinglePath(start, end);
    }

    @Override
    public MapEntry findNearestMapEntry(double latitude, double longitude) {
        System.out.println("lat: " + latitude + " lon: " + longitude);
        System.out.println("repo: " + (mapEntryRepository != null));
        System.out.println("cp: " + (configurationProvider != null));

        EndResult<MapEntry> entriesWithinDistance = mapEntryRepository.findWithinDistance("MapEntryLocation", latitude,
                longitude, configurationProvider.getSearchRadius());

        Iterator<MapEntry> entryIterator = entriesWithinDistance.iterator();

        while (entryIterator.hasNext()) {
            return entryIterator.next();
        }

        return null;
    }
}
