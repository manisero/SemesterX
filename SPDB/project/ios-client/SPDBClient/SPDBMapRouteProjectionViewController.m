#import "SPDBMapEntry.h"
#import "SPDBMapRouteProjectionViewController.h"

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
    int routeLength = [self.route count];
    CLLocationCoordinate2D coordinates[routeLength];
    
    for (int i = 0; i < routeLength; ++i)
    {
        coordinates[i] = [self convertMapEntry:[self.route objectAtIndex:i]];
    }
    
    MKPolyline *polyline = [MKPolyline polylineWithCoordinates:coordinates count:routeLength];
    [self.mapView removeOverlays:self.mapView.overlays];
    [self.mapView addOverlay:polyline];
}

- (CLLocationCoordinate2D)convertMapEntry:(SPDBMapEntry *)mapEntry
{
    return CLLocationCoordinate2DMake([mapEntry.latitude doubleValue], [mapEntry.longitude doubleValue]);
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
        polylineRenderer.lineWidth = 7.0;
        return polylineRenderer;
    }
    else
    {
        return  nil;
    }
}

@end
