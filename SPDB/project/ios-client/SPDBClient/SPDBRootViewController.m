#import "SPDBMapSelectionViewController.h"
#import "SPDBRootViewController.h"

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
    return section == 0 ? 2 : 1;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    NSString *identifier = [indexPath section] == 1 ? @"SearchRoute" : [indexPath row] == 0 ? @"PointFrom" : @"PointTo";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:identifier forIndexPath:indexPath];
    
    if ([identifier isEqualToString:@"PointFrom"])
    {
        cell.detailTextLabel.text = [self formatPoint:self.pointFrom];
    }
    else if ([identifier isEqualToString:@"PointTo"])
    {
        cell.detailTextLabel.text = [self formatPoint:self.pointTo];
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
}

- (void)didSelectPoint:(NSValue *)point atIndex:(NSUInteger)index
{
    NSLog(@"KODZINK :-)");
    
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

@end
