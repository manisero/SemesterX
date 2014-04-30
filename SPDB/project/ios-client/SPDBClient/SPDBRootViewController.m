#import <PXAlertView/PXAlertView.h>
#import "SPDBMapEntry.h"
#import "SPDBMapSelectionViewController.h"
#import "SPDBObjectManagerFactory.h"
#import "SPDBRootViewController.h"
#import "SPDBMapRouteProjectionViewController.h"

@interface SPDBRootViewController ()

@end

@implementation SPDBRootViewController

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 2;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    if (section == 0)
    {
        return 3;
    }
    else if (section == 1)
    {
        return 1;
    }
    
    return 0;
}

- (CGFloat)tableView:(UITableView *)tableView heightForRowAtIndexPath:(NSIndexPath *)indexPath
{
    if ([indexPath section] == 0)
    {
        if ([indexPath row] == 2)
        {
            return 216.0;
        }
    }
         
    return self.tableView.rowHeight;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    NSString *identifier = [self fieldIdentifierForIndexPath:indexPath];
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:identifier forIndexPath:indexPath];
    cell = [self configureCell:cell forPath:indexPath];
    
    return cell;
}

- (NSString *)fieldIdentifierForIndexPath:(NSIndexPath *)indexPath
{
    if ([indexPath section] == 0)
    {
        if ([indexPath row] == 0)
        {
            return @"PointFrom";
        }
        else if ([indexPath row] == 1)
        {
            return @"PointTo";
        }
        else if ([indexPath row] == 2)
        {
            return @"ArrivalTime";
        }
    }
    else if ([indexPath section] == 1)
    {
        if ([indexPath row] == 0)
        {
            return @"SearchRoute";
        }
    }
    
    [NSException raise:NSInvalidArgumentException format:@"No identifier for path: %@", indexPath];
    return nil;
}

- (UITableViewCell *)configureCell:(UITableViewCell *)cell forPath:(NSIndexPath *)indexPath
{
    if ([indexPath section] == 0)
    {
        if ([indexPath row] == 0)
        {
            cell.detailTextLabel.text = [self formatPoint:self.pointFrom];
        }
        else if ([indexPath row] == 1)
        {
            cell.detailTextLabel.text = [self formatPoint:self.pointTo];
        }
    }
    
    return cell;
}

- (NSString *)formatPoint:(NSValue *)point
{
    if (point != nil)
    {
        CGPoint pointStructure = [point CGPointValue];
        return [NSString stringWithFormat:@"[%.2f, %.2f]", pointStructure.x, pointStructure.y];
    }
    
    return @"(undefined)";
}

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    if ([indexPath section] == 0)
    {
        self.selectedPoint = [indexPath row] == 0 ? self.pointFrom : self.pointTo;
        [self performSegueWithIdentifier:@"PickLocation" sender:self];
    }
}

- (BOOL)tableView:(UITableView *)tableView canEditRowAtIndexPath:(NSIndexPath *)indexPath
{
    return NO;
}

- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    if ([[segue identifier] isEqualToString:@"PickLocation"])
    {
        SPDBMapSelectionViewController *destinationViewController = [segue destinationViewController];
        destinationViewController.delegate = self;
        destinationViewController.initialPoint = self.selectedPoint;
        destinationViewController.pointIndex = self.selectedPoint == self.pointFrom ? 0 : 1;
    }
    else if ([[segue identifier] isEqualToString:@"ShowRoute"])
    {
        SPDBMapRouteProjectionViewController *destinationViewController = [segue destinationViewController];
        destinationViewController.route = self.foundRoute;
        destinationViewController.arrivalTime = self.selectedArrivalTime;
    }
}

- (void)didSelectPoint:(NSValue *)point atIndex:(NSUInteger)index
{    
    if (index == 0)
    {
        self.pointFrom = point;
    }
    else if (index == 1)
    {
        self.pointTo = point;
    }
    
    [self.tableView reloadData];
}

- (IBAction)searchButtonPressed:(id)sender
{
    if (self.pointFrom == nil || self.pointTo == nil)
    {
        [self showValidationFailedAlert];
        return;
    }
    
    [self updateSelectedDate];
    [self downloadRoute];
}

- (void)showValidationFailedAlert
{
    [PXAlertView showAlertWithTitle:@"Validation failed"
                            message:@"Please select points before searching route."
                        cancelTitle:@"OK"
                         completion:nil];
}

- (void)updateSelectedDate
{
    UIView *timePickerContentView = [[self.tableView cellForRowAtIndexPath:[NSIndexPath indexPathForRow:2 inSection:0]] contentView];
    
    for (id subview in timePickerContentView.subviews)
    {
        if ([subview isKindOfClass:[UIDatePicker class]])
        {
            UIDatePicker *datePicker = subview;
            self.selectedArrivalTime = datePicker.date;
        }
    }
}

- (void)downloadRoute
{
    [self setUpHud];
    [self fetchShortestPath];
}

- (void)setUpHud
{
    self.progressHud = [MBProgressHUD showHUDAddedTo:self.view animated:YES];
    self.progressHud.mode = MBProgressHUDModeAnnularDeterminate;
    self.progressHud.labelText = @"Loading";
}

- (void)fetchShortestPath
{
    SPDBShortestPathFetcher *shortestPathFetcher = [SPDBShortestPathFetcher new];
    SPDBMapEntry *entryFrom = [self mapEntryFromPoint:self.pointFrom];
    SPDBMapEntry *entryTo = [self mapEntryFromPoint:self.pointTo];
    
    [shortestPathFetcher fetchShortestPathFromEntry:entryFrom toEntry:entryTo delegate:self];
}

- (SPDBMapEntry *)mapEntryFromPoint:(NSValue *)point
{
    CGPoint pointStructure = [point CGPointValue];
    
    return [SPDBMapEntry mapEntryWithLatitude:[NSNumber numberWithDouble:pointStructure.x] andLongitude:[NSNumber numberWithDouble:pointStructure.y]];
}

- (void)updateProgress:(CGFloat)progress
{
    self.progressHud.progress = progress;
}

- (void)didFetchShortestPath:(NSArray *)shortestPath
{
    [self.progressHud hide:YES];
    [self setFoundRoute:shortestPath];
    [self performSegueWithIdentifier:@"ShowRoute" sender:self];
}

- (void)didFailFetchingShortestPath:(NSError *)error
{
    [self.progressHud hide:YES];
    [self showRouteFetchFailedAlert];
}

- (void)showRouteFetchFailedAlert
{
    [PXAlertView showAlertWithTitle:@"Route fetch failed"
                            message:@"Could not fetch route. Please check your Internet connection."
                        cancelTitle:@"OK"
                         completion:nil];
}

@end
