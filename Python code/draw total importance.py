# Draw the feature importance chart
from matplotlib import pyplot as plt
import matplotlib.ticker as mtick

# Set font properties
font = {'family': 'serif',
        'serif': 'Times New Roman',  # Change to Times New Roman
        'weight': 'normal',
        'size': 14}
plt.rc('font', **font)

# Data
A = ["D1", "D2", "D12", "AT", "D4", "D3", "D5", "D0.5", "D10", "D9", "D6", "SR", "WS", "D7", "D8"]
B = [0.549/7.411, 0.530/7.411, 0.521/7.411, 0.512/7.411, 0.502/7.411,
     0.498/7.411, 0.494/7.411, 0.491/7.411, 0.487/7.411, 0.477/7.411,
     0.476/7.411, 0.470/7.411, 0.469/7.411, 0.468/7.411, 0.467/7.411]
a = A[::-1]  # Reverse the order of A for plotting
b = B[::-1]  # Reverse the order of B for plotting
height = 0.6  # Bar height

# Set bar colors to 'red'
colors = ['red'] * 15  # Set all bar colors to 'red'

# Draw a horizontal bar chart
plt.barh(range(len(a)), b, height=height, tick_label=a, label="CPT", color=colors, alpha=1, edgecolor='black')

# Add text labels to display the values
for x, y in enumerate(b):
    formatted_value = "{:.4f}".format(y)  # Keep four decimal places
    plt.text(y + 0.01, x, formatted_value, verticalalignment='center', horizontalalignment='left')

# Set the range of the x-axis
plt.xlim((0.00, 0.12))

# Format x-axis to display two decimal places
ax = plt.gca()
ax.xaxis.set_major_formatter(mtick.FormatStrFormatter('%.2f'))

# Set tick direction to inward
plt.tick_params(axis='both', direction='in', labelsize=14)

# Set axis labels
plt.ylabel("Variable", fontsize=16)
plt.xlabel("Feature importance", fontsize=16)

# Set border width
awith = 1.5  # Set the width of the bottom and left borders to 1.5
bwith = 1.5  # Set the width of the top and right borders to 1.5
ax = plt.gca()  # Get the current axes

# Adjust border widths
ax.spines['bottom'].set_linewidth(awith)
ax.spines['left'].set_linewidth(awith)
ax.spines['top'].set_linewidth(3)  # Set the top border width to 3
ax.spines['right'].set_linewidth(bwith)

# Save the chart as an image
plt.savefig('total_importance.jpg', bbox_inches='tight', dpi=600, pad_inches=0.0)

# Show the chart
plt.show()
