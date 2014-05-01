package pl.edu.pw.elka.spdb.services;

import org.springframework.beans.factory.annotation.Autowired;
import pl.edu.pw.elka.spdb.adapters.RouteListAdapter;
import pl.edu.pw.elka.spdb.dao.entries.IMapEntryDAO;
import pl.edu.pw.elka.spdb.model.MapEntry;
import pl.edu.pw.elka.spdb.model.Route;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;
import java.util.List;

@Path("/entry")
public class MapEntryService {
    @Autowired
    private IMapEntryDAO mapEntryDAO;

    @GET
    @Path("/nearest/{latitude}/{longitude}")
    @Produces("application/json")
    public Response getNearestMapEntries(@PathParam("latitude") double latitude,
                                         @PathParam("longitude") double longitude) {
        MapEntry nearestMapEntry = mapEntryDAO.findNearestMapEntry(latitude, longitude);

        Response response = Response.ok().entity(nearestMapEntry).build();

        return response;
    }

    @GET
    @Path("/shortestPath/{startingNodeId}/{finishingNodeId}")
    @Produces("application/json")
    public Response getShortestPath(@PathParam("startingNodeId") long startingNodeId,
                                    @PathParam("finishingNodeId") long finishingNodeId) {
        MapEntry startingNode = mapEntryDAO.findMapEntryById(startingNodeId);
        MapEntry finishingNode = mapEntryDAO.findMapEntryById(finishingNodeId);
        List<Route> shortestPath = mapEntryDAO.findFastestRoute(startingNode, finishingNode);

        Response response = Response.ok().entity(new RouteListAdapter(shortestPath)).build();

        return response;
    }
}
