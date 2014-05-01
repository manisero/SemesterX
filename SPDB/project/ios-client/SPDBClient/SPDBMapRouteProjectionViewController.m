#import "SPDBMapEntry.h"
#import "SPDBMapRouteProjectionViewController.h"
#import "SPDBRoute.h"

@interface SPDBMapRouteProjectionViewController ()

@end

@implementation SPDBMapRouteProjectionViewController

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    [self setMapRegionToWarsaw];
    [self drawRoute];
}

- (void)setMapRegionToWarsaw
{
    MKCoordinateRegion warsaw = MKCoordinateRegionMake(CLLocationCoordinate2DMake(52.232091, 21.006154), MKCoordinateSpanMake(0.04, 0.3));
    [self.mapView setRegion:warsaw animated:YES];
}

- (void)drawRoute
{
    NSArray *coordinatesAsObjects = [self convertRouteToCoordinates:self.route];
    int routeLength = [coordinatesAsObjects count];
    CLLocationCoordinate2D coordinates[routeLength];
    
    for (int i = 0; i < routeLength; ++i)
    {
        CLLocation *location = [coordinatesAsObjects objectAtIndex:i];
        coordinates[i] = [location coordinate];
    }
    
    MKPolyline *polyline = [MKPolyline polylineWithCoordinates:coordinates count:routeLength];
    [self.mapView removeOverlays:self.mapView.overlays];
    [self.mapView addOverlay:polyline];
}

- (NSArray *)convertRouteToCoordinates:(NSArray *)route
{
    NSMutableArray *coordinates = [NSMutableArray new];
    
    for (int i = 0; i < [route count]; ++i)
    {
        SPDBRoute *routeSegment = [route objectAtIndex:i];
        [coordinates addObject:[self mapEntryToLocation:routeSegment.routeFrom]];
    }
    
    if ([route count] > 0)
    {
        SPDBRoute *lastSegment = [route lastObject];
        [coordinates addObject:[self mapEntryToLocation:lastSegment.routeTo]];
    }
    
    return coordinates;
}

- (CLLocation *)mapEntryToLocation:(SPDBMapEntry *)mapEntry
{
    return [[CLLocation alloc] initWithLatitude:[mapEntry.latitude doubleValue]
                                      longitude:[mapEntry.longitude doubleValue]];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
}

- (MKOverlayRenderer *)mapView:(MKMapView *)mapView rendererForOverlay:(id<MKOverlay>)overlay
{
    if([overlay isKindOfClass:[MKPolyline class]])
    {
        MKPolylineRenderer *polylineRenderer = [[MKPolylineRenderer alloc] initWithPolyline:overlay];
        polylineRenderer.strokeColor = [[UIColor colorWithRed:0.5 green:0.0 blue:0.5 alpha:1.0] colorWithAlphaComponent:1.0];
        polylineRenderer.lineWidth = 5.0;
        return polylineRenderer;
    }
    else
    {
        return  nil;
    }
}

@end
